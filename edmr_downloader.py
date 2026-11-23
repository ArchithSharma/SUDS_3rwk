"""
EDMR CSV bulk downloader
------------------------
SETUP:
    Just install Python (python.org). On first run this script installs
    playwright, pandas, openpyxl and the Chromium browser automatically.

USAGE:
    python edmr_downloader.py
    You will be prompted for permit numbers (pasted or from a file) and dates.
"""
import glob
import importlib
import importlib.util
import re
import subprocess
import sys
import threading
import time
from datetime import datetime
from pathlib import Path


def ensure_packages():
    """Install any missing Python packages into the interpreter running this script."""
    needed = {"playwright": "playwright", "pandas": "pandas", "openpyxl": "openpyxl"}
    missing = [pip_name for mod, pip_name in needed.items() if importlib.util.find_spec(mod) is None]
    if missing:
        print(f"Installing missing packages: {', '.join(missing)} ...")
        subprocess.check_call([sys.executable, "-m", "pip", "install", *missing])
        importlib.invalidate_caches()


def install_chromium():
    print("Installing Chromium browser for Playwright (one-time, may take a minute)...")
    subprocess.check_call([sys.executable, "-m", "playwright", "install", "chromium"])


ensure_packages()

from playwright.sync_api import sync_playwright, TimeoutError as PWTimeout  # noqa: E402

# Configuration

REPORT_URL = "http://cedatareporting.pa.gov/Reportserver/Pages/ReportViewer.aspx?/Public/DEP/CW/SSRS/EDMR"
HEADLESS = True  # TODO: set HEADLESS to False if not working

VIEW_REPORT_WAIT_MS = 30000  # fixed wait after clicking "View Report"
# TODO: 30 seconds is safe; 25 might work.

# Timing profiles (milliseconds / counts). The retry pass is slower and more patient.
FIRST_PASS = dict(
    name="FIRST PASS",
    wait_after_type_ms=5000,
    wait_after_export_ms=1000,
    load_timeout_ms=60000,
    download_timeout_ms=120000,
    click_retries=30,
)
RETRY_PASS = dict(
    name="RETRY PASS",
    wait_after_type_ms=10000,
    wait_after_export_ms=2000,
    load_timeout_ms=120000,
    download_timeout_ms=180000,
    click_retries=50,
)

# Input parsing and prompting


def parse_permits_from_text(text):
    """Pull unique permit numbers (tokens starting with P) out of pasted text."""
    found = re.findall(r"\bP[A-Z0-9]+\b", text.upper())
    return list(dict.fromkeys(found))  # de-duplicate, keep order


def read_permits_from_file(path, column):
    import pandas as pd

    path = Path(path.strip().strip('"').strip("'"))
    if not path.exists():
        raise FileNotFoundError(f"File not found: {path}")
    if path.suffix.lower() in (".xlsx", ".xlsm", ".xls"):
        df = pd.read_excel(path, dtype=str)
    else:
        df = pd.read_csv(path, dtype=str)
    if column not in df.columns:
        raise KeyError(f"Column '{column}' not found. Available columns: {list(df.columns)}")
    values = df[column].dropna().astype(str).str.strip()
    return list(dict.fromkeys(v for v in values if v))


def prompt_permits():
    print("How do you want to provide permit numbers?")
    print("  1) Paste a list of permit numbers")
    print("  2) Read them from a file (CSV or Excel)")
    while True:
        choice = input("Enter 1 or 2: ").strip()
        if choice in ("1", "2"):
            break

    while True:
        try:
            if choice == "1":
                print("\nPaste your permit numbers (any separator; each must start with 'P').")
                print("Press Enter on an empty line when done:")
                lines = []
                while True:
                    line = input()
                    if not line.strip():
                        break
                    lines.append(line)
                permits = parse_permits_from_text("\n".join(lines))
            else:
                path = input("\nPath to file: ")
                column = input("Permit column name [PERMIT]: ").strip() or "PERMIT"
                permits = read_permits_from_file(path, column)
            if permits:
                return permits
            print("No permit numbers found. Try again.")
        except Exception as e:
            print(f"Error: {e}")
            if choice == "1":
                continue


def prompt_date(label):
    while True:
        value = input(f"{label} (MM/DD/YYYY): ").strip()
        try:
            datetime.strptime(value, "%m/%d/%Y")
            return value
        except ValueError:
            print("  Invalid date. Use MM/DD/YYYY, e.g. 01/01/2021.")


def prompt_output_dir():
    """Relative names are created next to this script (not wherever the terminal happens to be)."""
    name = input("Output folder name [edmr_csvs]: ").strip().strip('"').strip("'")
    path = Path(name or "edmr_csvs").expanduser()
    if not path.is_absolute():
        try:
            base = Path(__file__).resolve().parent
        except NameError:  # running in a notebook
            base = Path.cwd()
        path = base / path
    return path


def prompt_dates():
    while True:
        begin = prompt_date("Begin date")
        end = prompt_date("End date")
        if datetime.strptime(begin, "%m/%d/%Y") <= datetime.strptime(end, "%m/%d/%Y"):
            return begin, end
        print("  Begin date must be on or before end date. Try again.\n")


# Page element locators and helpers


def find_in_any_frame(page, strategies):
    """Try each locator strategy across the main page and every frame; return first match."""
    for frame in [page] + list(page.frames):
        for strategy in strategies:
            try:
                loc = strategy(frame)
                if loc.count() > 0:
                    return loc.first
            except Exception:
                continue
    return None


def get_begin_date_input(page):
    return find_in_any_frame(page, [
        lambda f: f.locator("input[id*='BeginDate' i]"),
        lambda f: f.locator("input[id*='Begin' i]"),
        lambda f: f.get_by_label("Monitoring Begin Date", exact=False),
        lambda f: f.get_by_label("Begin Date", exact=False),
    ])


def get_end_date_input(page):
    return find_in_any_frame(page, [
        lambda f: f.locator("input[id*='EndDate' i]"),
        lambda f: f.locator("input[id*='End' i]"),
        lambda f: f.get_by_label("Monitoring End Date", exact=False),
        lambda f: f.get_by_label("End Date", exact=False),
    ])


def get_permit_box(page):
    return find_in_any_frame(page, [
        lambda f: f.locator("input[id*='Permit' i]"),
        lambda f: f.get_by_label("Permit", exact=False),
    ])


def get_permit_null_checkbox(page, permit_box):
    """The checkbox that must be UNCHECKED to allow typing a permit number."""
    for xp in (
        "xpath=../input[@type='checkbox']",
        "xpath=../preceding-sibling::*[1]//input[@type='checkbox']",
        "xpath=ancestor::td[1]//input[@type='checkbox']",
        "xpath=ancestor::tr[1]//input[@type='checkbox']",
        "xpath=ancestor::div[1]//input[@type='checkbox']",
    ):
        try:
            candidate = permit_box.locator(xp)
            if candidate.count() > 0:
                return candidate.first
        except Exception:
            continue

    return find_in_any_frame(page, [
        lambda f: f.locator("input[type='checkbox'][id*='Permit' i]"),
        lambda f: f.locator("input[type='checkbox'][id*='Null' i]"),
    ])


def get_view_report_button(page):
    return find_in_any_frame(page, [
        lambda f: f.get_by_role("button", name="View Report", exact=False),
        lambda f: f.locator("input[value='View Report']"),
        lambda f: f.locator("[title*='View Report' i]"),
        lambda f: f.locator("a:has-text('View Report')"),
        lambda f: f.locator("button:has-text('View Report')"),
    ])


def get_export_button(page):
    return find_in_any_frame(page, [
        lambda f: f.locator("[title*='Export' i]"),
        lambda f: f.locator("[title*='Save' i]"),
        lambda f: f.locator("a[id*='Export' i]"),
        lambda f: f.locator("div[id*='Export' i]"),
        lambda f: f.locator("[aria-label*='Export' i]"),
        lambda f: f.locator("[aria-label*='Save' i]"),
    ])


def get_csv_menu_item(page):
    return find_in_any_frame(page, [
        lambda f: f.get_by_text("CSV (comma delimited)", exact=False),
        lambda f: f.get_by_text("CSV", exact=False),
        lambda f: f.locator("a:has-text('CSV')"),
        lambda f: f.locator("[title*='CSV' i]"),
        lambda f: f.locator("a[role='menuitem'][title*='CSV' i]"),
    ])


def wait_for(page, finder, timeout_ms, poll_ms=500):
    """Poll finder(page) until it returns something or the timeout passes."""
    deadline = time.time() + timeout_ms / 1000
    result = None
    while time.time() < deadline:
        result = finder(page)
        if result:
            return result
        page.wait_for_timeout(poll_ms)
    return result


def click_export_until_csv_menu(page, export_btn, *, retries, menu_timeout_ms):
    for attempt in range(1, retries + 1):
        print(f"  [5/5] Export click attempt {attempt}/{retries}...")
        try:
            export_btn.evaluate("el => el.click()")
        except Exception as e:
            print(f"  ! Export click failed: {e}")
            page.wait_for_timeout(1000)
            continue

        csv_item = wait_for(page, get_csv_menu_item, menu_timeout_ms)
        if csv_item:
            return csv_item
        print("  ! CSV menu did not appear")
        page.wait_for_timeout(1000)
    return None


def click_until_download(page, locator, *, retries, download_timeout_ms):
    for attempt in range(1, retries + 1):
        print(f"  [5/5] CSV click attempt {attempt}/{retries}...")
        try:
            with page.expect_download(timeout=download_timeout_ms) as info:
                locator.evaluate("el => el.click()")
            return info.value
        except Exception as e:
            print(f"  ! CSV click did not start a download: {e}")
            page.wait_for_timeout(500)
    return None


# Flow


def fill_field(field, value):
    field.click()
    field.fill("")
    field.fill(value)


def download_one_permit(page, permit, begin_date, end_date, cfg, output_dir):
    print(f"\n=== {permit} ===")
    page.goto(REPORT_URL, wait_until="networkidle")

    # Dates
    for label, getter, value in (
        ("Begin", get_begin_date_input, begin_date),
        ("End", get_end_date_input, end_date),
    ):
        field = getter(page)
        if field:
            fill_field(field, value)
        else:
            print(f"  ! Could not find {label} Date field (continuing -- it may default correctly).")

    # Uncheck the null checkbox, then type the permit number
    permit_box = get_permit_box(page)
    if not permit_box:
        print("  X Could not find the Permit # text box.")
        return False

    checkbox = get_permit_null_checkbox(page, permit_box)
    if checkbox:
        try:
            if checkbox.is_checked():
                checkbox.click()
                page.wait_for_timeout(300)
        except Exception as e:
            print(f"  ! Could not read/click the null checkbox ({e}), trying to type anyway.")
    else:
        print("  ! Could not find a null checkbox near Permit # -- trying to type anyway.")

    permit_box = get_permit_box(page) or permit_box  # may be replaced after unchecking
    try:
        permit_box.click()
        permit_box.fill("")
        permit_box.type(permit, delay=20)
        print(f"  [3/5] Typed permit number: {permit}")
        page.wait_for_timeout(cfg["wait_after_type_ms"])
    except Exception as e:
        print(f"  X Could not type into the Permit # box: {e}")
        return False

    # Run the report
    view_btn = get_view_report_button(page)
    if not view_btn:
        print("  X Could not find the 'View Report' button.")
        return False
    print("  [4/5] Clicking 'View Report'...")
    view_btn.click()

    page.wait_for_timeout(VIEW_REPORT_WAIT_MS)
    print(f"  [4/5] Waited {VIEW_REPORT_WAIT_MS}ms after clicking 'View Report'")

    try:
        page.wait_for_load_state("networkidle", timeout=cfg["load_timeout_ms"])
        print("  [4/5] Report load complete (networkidle)")
    except PWTimeout:
        print("  [4/5] Report load timed out, continuing anyway...")

    page.wait_for_timeout(cfg["wait_after_export_ms"])

    # Export as CSV
    export_btn = wait_for(page, get_export_button, cfg["load_timeout_ms"], poll_ms=1000)
    if not export_btn:
        print("  X Could not find the Export/Save (floppy disk) button.")
        return False
    print("  [5/5] Found export button, clicking until CSV menu appears...")

    csv_item = click_export_until_csv_menu(
        page, export_btn,
        retries=cfg["click_retries"],
        menu_timeout_ms=cfg["wait_after_export_ms"] + 2000,
    )
    if not csv_item:
        print("  X Could not get the CSV menu to appear.")
        return False

    print("  [5/5] Found CSV menu item, clicking until download starts...")
    download = click_until_download(
        page, csv_item,
        retries=cfg["click_retries"],
        download_timeout_ms=cfg["download_timeout_ms"],
    )
    if not download:
        print("  X CSV menu item found, but no download started.")
        return False

    dest = output_dir / f"{permit}.csv"
    download.save_as(dest)
    print(f"  Saved -> {dest}")
    return True


def run_pass(page, permits, begin_date, end_date, cfg, output_dir):
    results = {}
    for permit in permits:
        print(f"\n--- {cfg['name']}: {permit} ---")
        try:
            ok = download_one_permit(page, permit, begin_date, end_date, cfg, output_dir)
        except Exception as e:
            print(f"  X ERROR on {permit}: {e}")
            ok = False
        results[permit] = ok
        time.sleep(1)
    return results


def browser_job(permits, begin_date, end_date, output_dir):
    """Runs the whole browser session. Returns (results, failed)."""
    with sync_playwright() as p:
        try:
            browser = p.chromium.launch(headless=HEADLESS)
        except Exception as e:
            if "Executable doesn't exist" not in str(e):
                raise
            install_chromium()
            browser = p.chromium.launch(headless=HEADLESS)
        context = browser.new_context(accept_downloads=True)
        page = context.new_page()

        results = run_pass(page, permits, begin_date, end_date, FIRST_PASS, output_dir)

        failed = [permit for permit, ok in results.items() if not ok]
        if failed:
            print(f"\n=== RETRY PASS ({len(failed)} permits) ===")
            results.update(run_pass(page, failed, begin_date, end_date, RETRY_PASS, output_dir))

        if not HEADLESS:
            print("\n(Leaving browser open for 15s so you can inspect the final state...)")
            page.wait_for_timeout(15000)

        browser.close()
    return results, failed


def run_in_thread(func, *args):
    """Playwright's sync API can't run inside an active asyncio loop (e.g. Jupyter),
    so run it in a separate thread, which works in notebooks and terminals alike."""
    box = {}

    def target():
        try:
            box["value"] = func(*args)
        except BaseException as e:
            box["error"] = e

    t = threading.Thread(target=target)
    t.start()
    t.join()
    if "error" in box:
        raise box["error"]
    return box["value"]


def combine_csvs(output_dir):
    import pandas as pd

    combined_path = output_dir / "combined_data.csv"
    frames = []
    for f in sorted(glob.glob(str(output_dir / "*.csv"))):
        if Path(f).name == combined_path.name:  # skip output from a previous run
            continue
        try:
            frames.append(pd.read_csv(f))
        except pd.errors.EmptyDataError:
            print(f"  ! Skipping empty file: {f}")
    if not frames:
        print("\nNo CSVs to combine.")
        return
    df = pd.concat(frames, ignore_index=True)
    df.to_csv(combined_path, index=False)
    print(f"\nCombined {len(frames)} file(s), {len(df)} rows -> {combined_path}")


def main():
    print("=== EDMR CSV bulk downloader ===\n")
    permits = prompt_permits()
    print(f"\nFound {len(permits)} unique permit(s).")
    begin_date, end_date = prompt_dates()
    output_dir = prompt_output_dir()
    print(f"\nDownloading {len(permits)} permit(s) from {begin_date} to {end_date} into {output_dir.resolve()}")

    output_dir.mkdir(parents=True, exist_ok=True)

    results, failed = run_in_thread(browser_job, permits, begin_date, end_date, output_dir)

    print("\n=== SUMMARY ===")
    for permit, ok in results.items():
        print(f"  {permit}: {'OK' if ok else 'FAILED'}")
    if failed:
        print(f"\nRetried {len(failed)} permit(s) with longer waits/timeouts.")

    combine_csvs(output_dir)


if __name__ == "__main__":
    main()