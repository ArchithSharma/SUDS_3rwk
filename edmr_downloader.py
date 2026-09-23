"""
EDMR CSV bulk downloader (v3)
-------------------------------
Flow, per your description of how the page actually works:
    1. Enter the (fixed) begin/end monitoring dates.
    2. Uncheck the "null"/blank checkbox next to Permit Number, which enables its
       text box, then type the permit number into it.
    3. Click the floppy-disk Export/Save button, then choose CSV -> file downloads.
    ...repeated for every permit number in your list.

SETUP (run once in a terminal):
    pip install playwright
    playwright install chromium

USAGE:
    1. Edit PERMIT_NUMBERS below with your list.
    2. Edit OUTPUT_DIR if you want a different save location.
    3. Run:
           python edmr_downloader.py
    4. Leave HEADLESS = False for your first run so you can watch it. If a step
       fails, it prints exactly which one -- screenshot the browser at that point
       and send it back so the matching selector can be fixed.

IF A STEP STILL FAILS:
    Run this in a terminal to record the exact clicks yourself, on the real page:
        playwright codegen "http://cedatareporting.pa.gov/Reportserver/Pages/ReportViewer.aspx?/Public/DEP/CW/SSRS/EDMR"
    Do the flow manually (dates -> uncheck Permit checkbox -> type permit # ->
    Export -> CSV). Playwright prints the exact selector for each click in real
    time -- copy those into the matching function below.
"""

import time
import pandas as pd
from pathlib import Path
from playwright.sync_api import sync_playwright, TimeoutError as PWTimeout

# ---------------- CONFIG ----------------

REPORT_URL = "http://cedatareporting.pa.gov/Reportserver/Pages/ReportViewer.aspx?/Public/DEP/CW/SSRS/EDMR"
OUTPUT_DIR = Path("./edmr_csvs")
HEADLESS = False  # set to True once you've confirmed it reliably works

PERMITTED_FACILITIES = pd.read_csv("permitted_facilities.csv")
PERMIT_NUMBERS = PERMITTED_FACILITIES["PERMIT"].dropna().unique().tolist()

BEGIN_DATE = "1/1/2021"
END_DATE = "9/21/2026"

# Timing configuration (milliseconds)
FIRST_PASS_WAIT_AFTER_TYPE_MS = 5000
FIRST_PASS_WAIT_BEFORE_EXPORT_MS = 1500
FIRST_PASS_WAIT_AFTER_EXPORT_MS = 1000
FIRST_PASS_LOAD_TIMEOUT_MS = 60000
FIRST_PASS_DOWNLOAD_TIMEOUT_MS = 120000
FIRST_PASS_CSV_CLICK_TIMEOUT_MS = 5000
FIRST_PASS_CSV_CLICK_RETRIES = 30

RETRY_WAIT_AFTER_TYPE_MS = 10000
RETRY_WAIT_BEFORE_EXPORT_MS = 4000
RETRY_WAIT_AFTER_EXPORT_MS = 2000
RETRY_LOAD_TIMEOUT_MS = 120000
RETRY_DOWNLOAD_TIMEOUT_MS = 180000
RETRY_CSV_CLICK_TIMEOUT_MS = 7000
RETRY_CSV_CLICK_RETRIES = 50

# ---------------- HELPERS ----------------


def find_in_any_frame(page, strategies):
    """Try each locator strategy across the main page and every frame; return first match."""
    frames = [page] + list(page.frames)
    for frame in frames:
        for strategy in strategies:
            try:
                loc = strategy(frame)
                if loc.count() > 0:
                    return loc.first
            except Exception:
                continue
    return None


def get_begin_date_input(page):
    strategies = [
        lambda f: f.locator("input[id*='BeginDate' i]"),
        lambda f: f.locator("input[id*='Begin' i]"),
        lambda f: f.get_by_label("Monitoring Begin Date", exact=False),
        lambda f: f.get_by_label("Begin Date", exact=False),
    ]
    return find_in_any_frame(page, strategies)


def get_end_date_input(page):
    strategies = [
        lambda f: f.locator("input[id*='EndDate' i]"),
        lambda f: f.locator("input[id*='End' i]"),
        lambda f: f.get_by_label("Monitoring End Date", exact=False),
        lambda f: f.get_by_label("End Date", exact=False),
    ]
    return find_in_any_frame(page, strategies)


def get_permit_box(page):
    strategies = [
        lambda f: f.locator("input[id*='Permit' i]"),
        lambda f: f.get_by_label("Permit", exact=False),
    ]
    return find_in_any_frame(page, strategies)


def get_permit_null_checkbox(page, permit_box):
    """The checkbox that must be UNCHECKED to allow typing a permit number."""
    strategies_relative = [
        "xpath=../input[@type='checkbox']",
        "xpath=../preceding-sibling::*[1]//input[@type='checkbox']",
        "xpath=ancestor::td[1]//input[@type='checkbox']",
        "xpath=ancestor::tr[1]//input[@type='checkbox']",
        "xpath=ancestor::div[1]//input[@type='checkbox']",
    ]
    for xp in strategies_relative:
        try:
            candidate = permit_box.locator(xp)
            if candidate.count() > 0:
                return candidate.first
        except Exception:
            continue

    # Fallback: any checkbox with "null" or "permit" in its id, anywhere in the doc
    return find_in_any_frame(page, [
        lambda f: f.locator("input[type='checkbox'][id*='Permit' i]"),
        lambda f: f.locator("input[type='checkbox'][id*='Null' i]"),
    ])


def get_view_report_button(page):
    strategies = [
        lambda f: f.get_by_role("button", name="View Report", exact=False),
        lambda f: f.locator("input[value='View Report']"),
        lambda f: f.locator("[title*='View Report' i]"),
        lambda f: f.locator("a:has-text('View Report')"),
        lambda f: f.locator("button:has-text('View Report')"),
    ]
    return find_in_any_frame(page, strategies)


def get_export_button(page):
    strategies = [
        lambda f: f.locator("[title*='Export' i]"),
        lambda f: f.locator("[title*='Save' i]"),
        lambda f: f.locator("a[id*='Export' i]"),
        lambda f: f.locator("div[id*='Export' i]"),
        lambda f: f.locator("[aria-label*='Export' i]"),
        lambda f: f.locator("[aria-label*='Save' i]"),
    ]
    return find_in_any_frame(page, strategies)


def wait_for_export_button(page, timeout_ms, poll_ms=1000):
    deadline = time.time() + timeout_ms / 1000
    last_button = None
    while time.time() < deadline:
        last_button = get_export_button(page)
        if last_button:
            return last_button
        page.wait_for_timeout(poll_ms)
    return last_button


def click_export_until_csv_menu(page, export_btn, *, retries, click_timeout_ms, menu_timeout_ms, retry_wait_ms=1000):
    for attempt in range(1, retries + 1):
        print(f"  [5/5] Export click attempt {attempt}/{retries}...")
        try:
            export_btn.evaluate("el => el.click()")
        except Exception as e:
            print(f"  ! Export click attempt {attempt} failed: {e}")
            if attempt < retries:
                page.wait_for_timeout(retry_wait_ms)
            continue

        csv_item = wait_for_csv_menu_item(page, menu_timeout_ms)
        if csv_item:
            return csv_item

        print(f"  ! CSV menu did not appear after export click attempt {attempt}")
        if attempt < retries:
            page.wait_for_timeout(retry_wait_ms)

    return None


def get_csv_menu_item(page):
    strategies = [
        lambda f: f.get_by_text("CSV (comma delimited)", exact=False),
        lambda f: f.get_by_text("CSV", exact=False),
        lambda f: f.locator("a:has-text('CSV')"),
        lambda f: f.locator("[title*='CSV' i]"),
        lambda f: f.locator("a[role='menuitem'][title*='CSV' i]"),
    ]
    return find_in_any_frame(page, strategies)


def wait_for_csv_menu_item(page, timeout_ms, poll_ms=500):
    deadline = time.time() + timeout_ms / 1000
    last_item = None
    while time.time() < deadline:
        last_item = get_csv_menu_item(page)
        if last_item:
            return last_item
        page.wait_for_timeout(poll_ms)
    return last_item


def click_until_download(page, locator, *, retries, click_timeout_ms, download_timeout_ms, retry_wait_ms=500):
    for attempt in range(1, retries + 1):
        print(f"  [5/5] CSV click attempt {attempt}/{retries}...")
        try:
            with page.expect_download(timeout=download_timeout_ms) as download_info:
                locator.evaluate("el => el.click()")
            return download_info.value
        except Exception as e:
            print(f"  ! CSV click attempt {attempt} did not start a download: {e}")
            if attempt < retries:
                page.wait_for_timeout(retry_wait_ms)
    return None


# ---------------- MAIN FLOW ----------------


def download_one_permit(
    page,
    permit_number,
    output_dir,
    *,
    wait_after_type_ms,
    wait_before_export_ms,
    wait_after_export_ms,
    load_timeout_ms,
    download_timeout_ms,
    csv_click_timeout_ms,
    csv_click_retries,
):
    print(f"\n=== {permit_number} ===")
    page.goto(REPORT_URL, wait_until="networkidle")

    # Step 1: dates
    begin_input = get_begin_date_input(page)
    if begin_input:
        begin_input.click()
        begin_input.fill("")
        begin_input.fill(BEGIN_DATE)
    else:
        print("  ! Could not find Begin Date field (continuing -- it may default correctly).")

    end_input = get_end_date_input(page)
    if end_input:
        end_input.click()
        end_input.fill("")
        end_input.fill(END_DATE)
    else:
        print("  ! Could not find End Date field (continuing -- it may default correctly).")

    # Step 2: uncheck the null checkbox, then type the permit number
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

    # Re-fetch in case the box was replaced/enabled after unchecking
    permit_box = get_permit_box(page) or permit_box
    try:
        permit_box.click()
        permit_box.fill("")
        permit_box.type(permit_number, delay=20)
        print(f"  [3/5] Typed permit number: {permit_number}")
        page.wait_for_timeout(wait_after_type_ms)
        print(f"  [3/5] Waited {wait_after_type_ms}ms after typing permit number")
    except Exception as e:
        print(f"  X Could not type into the Permit # box: {e}")
        return False

    # Step 3: run the report, then export as CSV
    view_btn = get_view_report_button(page)
    if not view_btn:
        print("  X Could not find the 'View Report' button.")
        return False
    print("  [4/5] Clicking 'View Report' button...")
    view_btn.click()

    # Wait a fixed 45 seconds after clicking View Report before doing anything else.
    page.wait_for_timeout(30000)
    print("  [4/5] Waited 30000ms after clicking 'View Report'")

    try:
        page.wait_for_load_state("networkidle", timeout=load_timeout_ms)
        print("  [4/5] Report load complete (networkidle)")
    except PWTimeout:
        print("  [4/5] Report load timed out, continuing anyway...")

    # Give the report/export controls extra time to become usable before we click export.
    page.wait_for_timeout(wait_after_export_ms)

    export_btn = wait_for_export_button(page, load_timeout_ms)
    if not export_btn:
        print("  X Could not find the Export/Save (floppy disk) button.")
        return False
    print("  [5/5] Found export button, clicking until CSV menu appears...")
    csv_item = click_export_until_csv_menu(
        page,
        export_btn,
        retries=csv_click_retries,
        click_timeout_ms=csv_click_timeout_ms,
        menu_timeout_ms=wait_after_export_ms + 2000,
    )
    if not csv_item:
        print("  X Could not get the CSV menu to appear after repeated export clicks.")
        return False

    print("  [5/5] Found CSV menu item, clicking until download starts...")
    download = click_until_download(
        page,
        csv_item,
        retries=csv_click_retries,
        click_timeout_ms=csv_click_timeout_ms,
        download_timeout_ms=download_timeout_ms,
    )
    if not download:
        print("  X CSV menu item was found, but no download started after repeated clicks.")
        return False

    output_dir.mkdir(parents=True, exist_ok=True)
    dest = output_dir / f"{permit_number}.csv"
    download.save_as(dest)
    print(f"  Saved -> {dest}")
    return True


def run_pass(page, permits, output_dir, *, wait_after_type_ms, wait_before_export_ms, wait_after_export_ms, load_timeout_ms, download_timeout_ms, pass_name):
    results = {}
    for permit in permits:
        print(f"\n--- {pass_name}: {permit} ---")
        try:
            ok = download_one_permit(
                page,
                permit,
                output_dir,
                wait_after_type_ms=wait_after_type_ms,
                wait_before_export_ms=wait_before_export_ms,
                wait_after_export_ms=wait_after_export_ms,
                load_timeout_ms=load_timeout_ms,
                download_timeout_ms=download_timeout_ms,
                csv_click_timeout_ms=FIRST_PASS_CSV_CLICK_TIMEOUT_MS if pass_name == "FIRST PASS" else RETRY_CSV_CLICK_TIMEOUT_MS,
                csv_click_retries=FIRST_PASS_CSV_CLICK_RETRIES if pass_name == "FIRST PASS" else RETRY_CSV_CLICK_RETRIES,
            )
        except Exception as e:
            print(f"  X ERROR on {permit}: {e}")
            ok = False
        results[permit] = ok
        time.sleep(1)
    return results


def main():
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=HEADLESS)
        context = browser.new_context(accept_downloads=True)
        page = context.new_page()

        print("\n=== FIRST PASS ===")
        first_pass_results = run_pass(
            page,
            PERMIT_NUMBERS,
            OUTPUT_DIR,
            wait_after_type_ms=FIRST_PASS_WAIT_AFTER_TYPE_MS,
            wait_before_export_ms=FIRST_PASS_WAIT_BEFORE_EXPORT_MS,
            wait_after_export_ms=FIRST_PASS_WAIT_AFTER_EXPORT_MS,
            load_timeout_ms=FIRST_PASS_LOAD_TIMEOUT_MS,
            download_timeout_ms=FIRST_PASS_DOWNLOAD_TIMEOUT_MS,
            pass_name="FIRST PASS",
        )

        retry_permits = [permit for permit, ok in first_pass_results.items() if not ok]
        retry_results = {}

        if retry_permits:
            print(f"\n=== RETRY PASS ({len(retry_permits)} permits) ===")
            retry_results = run_pass(
                page,
                retry_permits,
                OUTPUT_DIR,
                wait_after_type_ms=RETRY_WAIT_AFTER_TYPE_MS,
                wait_before_export_ms=RETRY_WAIT_BEFORE_EXPORT_MS,
                wait_after_export_ms=RETRY_WAIT_AFTER_EXPORT_MS,
                load_timeout_ms=RETRY_LOAD_TIMEOUT_MS,
                download_timeout_ms=RETRY_DOWNLOAD_TIMEOUT_MS,
                pass_name="RETRY PASS",
            )

        if not HEADLESS:
            print("\n(Leaving browser open for 15s so you can inspect the final state...)")
            page.wait_for_timeout(15000)

        browser.close()

    print("\n=== SUMMARY ===")
    all_results = {**first_pass_results, **retry_results}
    for permit, ok in all_results.items():
        print(f"  {permit}: {'OK' if ok else 'FAILED'}")
    if retry_permits:
        print(f"\nRetried {len(retry_permits)} permits with longer waits/timeouts.")


if __name__ == "__main__":
    main()