#!/usr/bin/env python3
"""MCP stdio <-> HTTP bridge for kiwi: each JSON-RPC line of stdin is posted
to the /mcp URL; the answer (if any) is written as one line on stdout.
Python standard library only. Usage: kiwi-mcp-bridge.py URL (token in
KIWI_API_TOKEN)."""
import json, os, sys, urllib.error, urllib.request

url, token = sys.argv[1], os.environ["KIWI_API_TOKEN"]
for line in sys.stdin:
    if not line.strip():
        continue
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
        msg = json.loads(body)
    except ValueError:                  # 202 (notification), or no kiwi
        try:
            rid = json.loads(line).get("id")
        except (ValueError, AttributeError):
            rid = None
        if rid is None:
            continue
        msg = {"jsonrpc": "2.0", "id": rid,
               "error": {"code": -32603, "message": f"no MCP answer from {url}"}}
    print(json.dumps(msg, ensure_ascii=False, separators=(",", ":")), flush=True)
