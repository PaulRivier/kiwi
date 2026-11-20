#!/usr/bin/env python3
"""MCP stdio <-> HTTP bridge for kiwi: each JSON-RPC line of stdin is posted
to the /mcp URL; the answer (if any) is written as one line on stdout.

The bridge adds two tools of its own, to change a page as a local file with
the tools of the agent: download_page writes the text of a page to
DIR/SOURCE/UID.md, upload_page sends it back (PUT with If-Match) and deletes
it. The token never leaves the bridge.

Python standard library only. Usage: kiwi-mcp-bridge.py URL [--dir DIR]
(token in KIWI_API_TOKEN; DIR is /tmp/kiwi-pages-USER by default)."""
import argparse, getpass, json, os, re, sys, urllib.error, urllib.parse, urllib.request

ap = argparse.ArgumentParser(description="MCP stdio bridge for kiwi")
ap.add_argument("url", help="the /mcp URL of the kiwi")
ap.add_argument("--dir", help="folder of the downloaded pages (default /tmp/kiwi-pages-USER)")
args = ap.parse_args()
url, token = args.url, os.environ["KIWI_API_TOKEN"]
if not url.rstrip("/").endswith("/mcp"):
    ap.error("the URL must end with /mcp")
api = url.rstrip("/")[:-len("mcp")] + "api/page/"
pages_dir = args.dir or f"/tmp/kiwi-pages-{getpass.getuser()}"
etags = {}                              # page -> ETag of its downloaded text
os.makedirs(pages_dir, mode=0o700, exist_ok=True)
if os.stat(pages_dir).st_uid != os.getuid():
    ap.error(f"{pages_dir} belongs to another user")

GUIDE = """

# Pages as local files (this connector)

- To change a page, prefer working on a local file with your own tools
  over edit_page and write_page: download_page writes its text to a file
  and answers the path, the number of lines and the size, without the
  text; upload_page sends the file back, then deletes it.
- Read the downloaded file whole only if it is small: under the size the
  guide of the user gives, or 20 KB if it gives none. Otherwise find the
  passages with your tools (`grep -n`...) and read only them.
- If upload_page answers that the page changed meanwhile, download it
  again (the file is replaced) and redo your changes.
"""

PAGE = {"type": "string", "description": "The page: SOURCE/UID."}
TOOLS = [
    {"name": "download_page",
     "description": "Writes the text of a page (header and content, as read_page gives it) "
                    "to a local file, and answers its path, its number of lines and its size, "
                    "not the text. Change the file with your own tools, then send it back with "
                    "upload_page. Downloading again replaces the file.",
     "inputSchema": {"type": "object", "properties": {"page": PAGE}, "required": ["page"]}},
    {"name": "upload_page",
     "description": "Sends back a page downloaded with download_page: the page gets the text "
                    "of its local file, which is then deleted. Fails if the page changed since "
                    "the download: download it again, and redo your changes.",
     "inputSchema": {"type": "object", "properties": {
         "page": PAGE,
         "message": {"type": "string",
                     "description": "Short description of the change, for the history."}},
         "required": ["page"]}}]


class ToolError(Exception):
    pass


def http(method, path, data=None, headers=None):
    """Status, headers and body of a request to the API of the pages"""
    req = urllib.request.Request(api + path, data=data, method=method,
                                 headers={"Authorization": "Bearer " + token, **(headers or {})})
    try:
        with urllib.request.urlopen(req) as r:
            return r.status, r.headers, r.read()
    except urllib.error.HTTPError as e:
        return e.code, e.headers, e.read()
    except OSError as e:
        raise ToolError(f"kiwi does not answer at {url}: {e}")


def locate(page):
    """URL path and local file of a page SOURCE/UID"""
    source, _, uid = str(page).partition("/")
    if not re.fullmatch(r"[^/.][^/]*", source) or not re.fullmatch(r"[A-Za-z0-9_-]{1,64}", uid):
        raise ToolError(f"not a page SOURCE/UID: {page}")
    return (urllib.parse.quote(source, safe="") + "/" + uid,
            os.path.join(pages_dir, source, uid + ".md"))


def download_page(page):
    path, file = locate(page)
    status, headers, body = http("GET", path)
    if status != 200:
        raise ToolError(body.decode(errors="replace"))
    os.makedirs(os.path.dirname(file), mode=0o700, exist_ok=True)
    with open(file, "wb") as f:
        f.write(body)
    etags[page] = headers["ETag"]
    lines = body.count(b"\n")
    return f"{file}\nlines: {lines}, size: {len(body) / 1024:.1f} KB\n"


def upload_page(page, message=None):
    path, file = locate(page)
    if page not in etags or not os.path.exists(file):
        raise ToolError(f"download {page} first with download_page")
    with open(file, "rb") as f:
        data = f.read()
    query = "?message=" + urllib.parse.quote(message) if message else ""
    status, headers, body = http("PUT", path + query, data, {
        "If-Match": etags[page], "Content-Type": "text/markdown; charset=utf-8"})
    answer = body.decode(errors="replace")
    if status == 412:
        raise ToolError(answer + "\nDownload the page again with download_page "
                        "(your file is replaced), then redo your changes.")
    if status != 200:
        raise ToolError(answer)
    os.remove(file)
    del etags[page]
    return answer


def call_tool(params):
    """Result of a call of a tool of the bridge"""
    a = params.get("arguments") or {}
    try:
        if params["name"] == "download_page":
            text = download_page(a.get("page"))
        else:
            text = upload_page(a.get("page"), a.get("message"))
        return {"content": [{"type": "text", "text": text}]}
    except (ToolError, OSError) as e:
        return {"content": [{"type": "text", "text": str(e)}], "isError": True}


def post(line):
    """The answer of kiwi to a JSON-RPC line, None for a notification"""
    req = urllib.request.Request(url, data=line.encode(), method="POST", headers={
        "Content-Type": "application/json",
        "Accept": "application/json, text/event-stream",
        "Authorization": "Bearer " + token})
    try:
        with urllib.request.urlopen(req) as r:
            body = r.read()
    except urllib.error.HTTPError as e:
        body = e.read()
    except OSError as e:
        print(f"kiwi does not answer at {url}: {e}", file=sys.stderr)
        body = b""
    try:
        return json.loads(body)
    except ValueError:                  # 202 (notification), or no kiwi
        return None


for line in sys.stdin:
    if not line.strip():
        continue
    try:
        msg = json.loads(line)
        method, rid = msg.get("method"), msg.get("id")
        params = msg.get("params") or {}
    except (ValueError, AttributeError):
        method, rid, params = None, None, {}
    if method == "tools/call" and params.get("name") in ("download_page", "upload_page"):
        answer = {"jsonrpc": "2.0", "id": rid, "result": call_tool(params)}
    else:
        answer = post(line)
        if answer is None:
            if rid is None:
                continue
            answer = {"jsonrpc": "2.0", "id": rid,
                      "error": {"code": -32603, "message": f"no MCP answer from {url}"}}
        result = answer.get("result")
        if isinstance(result, dict) and method == "initialize":
            result["instructions"] = result.get("instructions", "") + GUIDE
        if isinstance(result, dict) and method == "tools/list":
            result["tools"] = result.get("tools", []) + TOOLS
    print(json.dumps(answer, ensure_ascii=False, separators=(",", ":")), flush=True)
