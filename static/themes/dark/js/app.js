// Kiwi theme script, loaded as a module (deferred, runs once the page is parsed)

const $ = (selector) => document.querySelector(selector);
const $$ = (selector) => document.querySelectorAll(selector);

const post = (url, params = {}) =>
  fetch(url, { method: "POST", body: new URLSearchParams(params) });


// Admin actions: the button is disabled until the server answers, then the
// page is reloaded, or an alert explains the failure (with the texts of the
// data-error-* attributes of the button)

async function adminAction(button, url, params) {
  const { errorSession, errorForbidden, errorFailed } = button.dataset;
  button.disabled = true;
  let response;
  try {
    response = await post(url, params);
  } catch {
    // network failure
    button.disabled = false;
    alert(errorFailed);
    return;
  }
  if (response.ok) {
    location.reload();
    return;
  }
  button.disabled = false;
  if (response.status === 401) alert(errorSession);
  else if (response.status === 403) alert(errorForbidden);
  else alert(`${errorFailed} (HTTP ${response.status})`);
}

// Logging out forgets the drafts of the editors that this browser keeps
// (EasyMDE autosave: "smde_" + uniqueId, and the header with the same
// prefix, see the editor below)
$("form[action='/logout']")?.addEventListener("submit", () => {
  try {
    for (const key of Object.keys(localStorage))
      if (key.startsWith("smde_kiwi-")) localStorage.removeItem(key);
  } catch {
    // no local storage: no draft
  }
});

$(".top-nav .reload")?.addEventListener("click", (e) =>
  adminAction(e.currentTarget, "/reload"));

// absent without editor in the config
$(".top-nav .edit-locally")?.addEventListener("click", (e) => {
  const { pageSource, pageId } = e.currentTarget.dataset;
  adminAction(e.currentTarget, "/edit-page",
              { "page-source": pageSource, "page-id": pageId });
});


// Edition from a block of a page: with the write right, the document has
// the URL of the editor, and its top-level blocks their line (data-line). A
// pencil in the right margin follows the block under the mouse, and opens
// the editor at its line.

const editable = $(".document[data-edit-url]");

if (editable) {
  const pencil = document.createElement("a");
  pencil.className = "icon edit-block";
  pencil.title = editable.dataset.editTitle;
  pencil.hidden = true;
  const img = document.createElement("img");
  img.src = "/static/images/edit.svg";
  img.alt = "";
  pencil.append(img);
  editable.append(pencil);

  editable.addEventListener("mouseover", (e) => {
    const block = e.target.closest("[data-line]");
    if (!block || block.parentElement !== editable) return;
    pencil.href = `${editable.dataset.editUrl}?line=${block.dataset.line}`;
    pencil.style.top = `${block.offsetTop}px`;
    pencil.hidden = false;
  });
}


// Suggestions for the header of a page, in a bar below its text area: the
// keys and the values of the vocabulary of its source (#meta-vocabulary,
// from kiwi), for the line of the cursor. Before the ":", its key; after
// it, the value being written, from the last "," of the line (access: the
// rights after a ":"). Tags, segment by segment: the children of the tag
// being written (groups "{...}" included), or of the tag written. Those
// that start with the text written come first, then those that contain it,
// without case nor accents. The bar opens when the user types (or
// Ctrl+Space), follows the cursor on its line, and closes on another line,
// with Escape, or when the text area loses the focus. ↓/↑ choose a
// suggestion, Tab or Enter write the chosen one (without a choice, they keep
// their role); a click writes one.

// without case nor accents (ligatures as their letters), nor spaces around
// the ">" of the tags
const normalized = (s) =>
  s.normalize("NFD").replace(/\p{M}/gu, "").toLowerCase().replace(/œ/g, "oe").replace(/æ/g, "ae")
    .replace(/\s*>\s*/g, ">").trim();

function headerSuggestions(meta, vocabulary, newValueText) {
  const entries = new Map(vocabulary.map((e) => [e.key, { ...e, norms: e.values.map(normalized) }]));
  const keys = vocabulary.map((e) => e.key);
  const keyNorms = keys.map(normalized);
  // the tags as a tree: the segments under each tag (normalized, "" for the
  // top), in the order of the vocabulary, with their normalized forms
  const children = new Map();
  for (const tag of entries.get("tags")?.values ?? []) {
    const segs = tag.split(">").map((seg) => seg.trim());
    const parent = normalized(segs.slice(0, -1).join(">"));
    if (!children.has(parent)) children.set(parent, { names: [], norms: [] });
    children.get(parent).names.push(segs.at(-1));
    children.get(parent).norms.push(normalized(segs.at(-1)));
  }
  const bar = document.createElement("div");
  bar.className = "meta-suggestions";
  bar.hidden = true;
  meta.after(bar);

  // open: the bar follows the cursor on this line (start in the text);
  // current: what a choice replaces in the text, and the suggestions
  let open = false, openLine = -1, current = null, selected = -1;

  const close = () => {
    open = false;
    current = null;
    bar.hidden = true;
  };

  // the texts that match what is written, and whether it is one of them
  const matching = (texts, norms, typed) => {
    const t = normalized(typed);
    const starts = [], contains = [];
    texts.forEach((x, i) => {
      if (norms[i].startsWith(t)) starts.push(x);
      else if (norms[i].includes(t)) contains.push(x);
    });
    return { found: starts.concat(contains), known: norms.includes(t) };
  };

  // the suggestions replacing the text from start to end, each one a label
  // and what it writes; isNew: the value written is in no page, nor the
  // start of one
  const show = (start, end, typed, items, isNew) => {
    const written = items.length === 1 && normalized(items[0].label) === normalized(typed);
    if ((items.length === 0 || written) && !isNew) {
      current = null;
      bar.hidden = true;
      return;
    }
    current = { start, end, items };
    selected = -1;
    const buttons = items.map((item, i) => {
      const b = document.createElement("button");
      b.type = "button";
      b.textContent = item.label;
      if (item.title) b.title = item.title;
      // the text area keeps the focus
      b.addEventListener("mousedown", (e) => e.preventDefault());
      b.addEventListener("click", () => choose(i));
      return b;
    });
    if (isNew) {
      const note = document.createElement("span");
      note.className = "meta-new";
      note.textContent = newValueText;
      buttons.push(note);
    }
    bar.replaceChildren(...buttons);
    bar.hidden = false;
  };

  const update = () => {
    const pos = meta.selectionStart, text = meta.value;
    const lineStart = text.lastIndexOf("\n", pos - 1) + 1;
    if (pos !== meta.selectionEnd || lineStart !== openLine) return close();
    let lineEnd = text.indexOf("\n", pos);
    if (lineEnd < 0) lineEnd = text.length;
    const line = text.slice(lineStart, lineEnd), at = pos - lineStart;
    const colon = line.indexOf(":");
    if (colon < 0 || at <= colon) {
      // the key, written with its ":" when the line has none
      const lead = line.length - line.trimStart().length;
      const typed = line.slice(lead, Math.max(lead, at));
      const { found } = matching(keys, keyNorms, typed);
      const items = found.map((k) => ({ label: k, text: colon < 0 ? `${k}: ` : k,
                                        title: entries.get(k).description }));
      return show(lineStart + lead, lineStart + (colon < 0 ? line.length : colon), typed, items, false);
    }
    const entry = entries.get(line.slice(0, colon).trim());
    if (!entry) return show(0, 0, "", [], false);
    if (entry.key === "tags") return tagSuggestions(lineStart, line, colon, at);
    const before = line.slice(colon + 1, at);
    // the value of the cursor, from the last "," before it to the next one,
    // without the spaces around
    const itemStart = colon + 1 + before.lastIndexOf(",") + 1;
    let itemEnd = line.indexOf(",", at);
    if (itemEnd < 0) itemEnd = line.length;
    const raw = line.slice(itemStart, itemEnd);
    const start = itemStart + raw.length - raw.trimStart().length;
    const end = Math.max(start, itemEnd - (raw.length - raw.trimEnd().length));
    const typed = line.slice(start, Math.max(start, at));
    const right = typed.indexOf(":");
    if (entry.rights && right >= 0) {
      const { found } = matching(entry.rights, entry.rights, typed.slice(right + 1));
      return show(lineStart + start + right + 1, lineStart + end, typed.slice(right + 1),
                  found.map((r) => ({ label: r, text: r })), false);
    }
    // a value that no page has, when nothing matches it
    const { found, known } = matching(entry.values, entry.norms, typed);
    show(lineStart + start, lineStart + end, typed, found.map((v) => ({ label: v, text: v })),
         typed.trim() !== "" && entry.values.length > 0 && !known && found.length === 0);
  };

  // the segment of the tag of the cursor: the prefixes of the groups open
  // before it, then the text since the last "," or brace, the last ">"
  // ending its parent
  const tagSuggestions = (lineStart, line, colon, at) => {
    const prefixes = [""];
    let text = "";
    for (const c of line.slice(colon + 1, at)) {
      if (c === "{") {
        prefixes.push(prefixes.at(-1) + text);
        text = "";
      } else if (c === "}") {
        if (prefixes.length > 1) prefixes.pop();
        text = "";
      } else if (c === ",") text = "";
      else text += c;
    }
    const segs = (prefixes.at(-1) + text).split(">");
    const typed = segs.pop().trimStart();
    const parent = normalized(segs.join(">"));
    // the segment, from its start to the next separator, without the spaces
    // after the cursor
    const start = at - typed.length;
    let end = at;
    while (end < line.length && !",{}>".includes(line[end])) end++;
    while (end > at && line[end - 1] === " ") end--;
    const siblings = children.get(parent) ?? { names: [], norms: [] };
    const t = normalized(typed);
    // a tag written, that has sub-tags: they follow it
    const i = siblings.norms.indexOf(t);
    const below = i >= 0 && children.get(parent === "" ? t : `${parent}>${t}`);
    if (below && line.slice(at, end).trim() === "") {
      const after = lineStart + start + typed.trimEnd().length;
      return show(after, lineStart + end, "",
                  below.names.map((seg) => ({ label: `> ${seg}`, text: ` > ${seg}` })), false);
    }
    const { found, known } = matching(siblings.names, siblings.norms, typed);
    show(lineStart + start, lineStart + end, typed, found.map((seg) => ({ label: seg, text: seg })),
         t !== "" && !known && found.length === 0);
  };

  const openHere = () => {
    open = true;
    openLine = meta.value.lastIndexOf("\n", meta.selectionStart - 1) + 1;
    update();
  };

  // the text written replaces the text of the suggestion (an input event:
  // the draft is kept, the bar follows)
  const choose = (i) => {
    const { start, end, items } = current;
    meta.setRangeText(items[i].text, start, end, "end");
    meta.dispatchEvent(new Event("input"));
  };

  const select = (i) => {
    selected = i;
    [...bar.querySelectorAll("button")].forEach((b, j) => {
      b.classList.toggle("selected", j === i);
      if (j === i) b.scrollIntoView({ block: "nearest" });
    });
  };

  meta.addEventListener("input", openHere);
  meta.addEventListener("blur", close);
  meta.addEventListener("click", () => { if (open) update(); });
  meta.addEventListener("keyup", (e) => {
    if (open && !["ArrowDown", "ArrowUp", "Tab", "Enter", "Escape"].includes(e.key)) update();
  });
  meta.addEventListener("keydown", (e) => {
    if (e.key === " " && e.ctrlKey) {
      e.preventDefault();
      openHere();
      return;
    }
    if (!current) return;
    const n = current.items.length;
    if (e.key === "Escape") {
      e.preventDefault();
      close();
    } else if (n > 0 && (e.key === "ArrowDown" || e.key === "ArrowUp")) {
      e.preventDefault();
      select(e.key === "ArrowDown" ? (selected + 1) % n : (selected <= 0 ? n : selected) - 1);
    } else if ((e.key === "Tab" || e.key === "Enter") && selected >= 0 && !e.shiftKey) {
      e.preventDefault();
      choose(selected);
    }
  });
}


// Page editor: the header of the page in a text area, its body in a
// markdown editor (EasyMDE, loaded before this script), saved with the
// hashes of the header and the body read; the same form creates a new page
// (class new-page: empty id and hashes, sent to /create-page)

const editorForm = $("form.page-editor");

if (editorForm && window.EasyMDE) {
  const data = editorForm.dataset;
  const field = (name) => editorForm.elements[name].value;
  const source = field("source"), id = field("id");
  const metaHash = field("meta-hash"), bodyHash = field("body-hash");
  const isNew = editorForm.classList.contains("new-page");
  const meta = editorForm.elements.meta;
  // titles of the toolbar buttons, from the data-button-NAME attributes
  const button = (name, action, text) => ({
    name, action, text, title: data[`button${name.replace(/(^|-)(.)/g, (_, _d, c) => c.toUpperCase())}`],
  });
  const E = window.EasyMDE;

  // the files sent from this editor, by uid: a URL of their bytes in the
  // browser, as kiwi serves a file only once a saved page links it
  const sentFiles = new Map();
  // the links and images of these files in a preview point to their bytes
  const showSentFiles = (preview) => {
    for (const [uid, url] of sentFiles) {
      for (const img of preview.querySelectorAll(`img[src^="/file/"][src$="/${uid}"]`)) img.src = url;
      for (const a of preview.querySelectorAll(`a[href^="/file/"][href$="/${uid}"]`)) a.href = url;
    }
  };

  // the preview is rendered by kiwi, as the page would be; the side by side
  // preview is rendered again once the typing stops
  let previewTimer;
  const renderPreview = (text, preview) => {
    clearTimeout(previewTimer);
    previewTimer = setTimeout(async () => {
      try {
        const response = await post("/preview", { source, id, body: text });
        preview.innerHTML = await response.text();
        showSentFiles(preview);
      } catch {
        // network failure: the preview is kept
      }
    }, 300);
    return preview.innerHTML;
  };

  // a draft is kept in the browser for this version of the body (and of
  // the header) only, or for the new pages of the source
  const editor = new E({
    element: editorForm.elements.body,
    autoDownloadFontAwesome: false,
    spellChecker: false,
    autosave: { enabled: true, delay: 5000,
                uniqueId: isNew ? `kiwi-new-${source}` : `kiwi-${source}/${id}/${bodyHash}` },
    maxHeight: "65vh",
    previewClass: ["editor-preview", "document"],
    previewRender: renderPreview,
    status: ["lines", "cursor"],
    toolbar: [
      button("bold", E.toggleBold, "B"),
      button("italic", E.toggleItalic, "I"),
      button("strikethrough", E.toggleStrikethrough, "S"),
      button("heading", E.toggleHeadingSmaller, "H"),
      "|",
      button("quote", E.toggleBlockquote, "\u275D"),
      button("code", E.toggleCodeBlock, "</>"),
      button("unordered-list", E.toggleUnorderedList, "\u2022"),
      button("ordered-list", E.toggleOrderedList, "1."),
      "|",
      button("link", E.drawLink, "[ ]"),
      button("image", E.drawImage, "![ ]"),
      button("table", E.drawTable, "\u25A6"),
      button("horizontal-rule", E.drawHorizontalRule, "\u2015"),
      "|",
      { ...button("preview", E.togglePreview, "\u25C9"), noDisable: true },
      { ...button("side-by-side", E.toggleSideBySide, "\u25EB"), noDisable: true, noMobile: true },
      { ...button("fullscreen", E.toggleFullScreen, "\u26F6"), noDisable: true, noMobile: true },
      "|",
      button("undo", E.undo, "\u21B6"),
      button("redo", E.redo, "\u21B7"),
      button("guide", "/commonmark", "?"),
    ],
  });

  // the draft of the header, with the prefix of those of EasyMDE (see the
  // logout above)
  const metaKey = isNew ? `smde_kiwi-meta-new-${source}` : `smde_kiwi-meta-${source}/${id}/${metaHash}`;
  const storage = {
    get: () => { try { return localStorage.getItem(metaKey); } catch { return null; } },
    set: (v) => { try { localStorage.setItem(metaKey, v); } catch { /* no draft */ } },
    clear: () => { try { localStorage.removeItem(metaKey); } catch { /* no draft */ } },
  };
  const metaDraft = storage.get();
  if (metaDraft !== null) meta.value = metaDraft;
  // the text area grows with the header
  const fitMeta = () => { meta.rows = Math.max(3, meta.value.split("\n").length + 1); };
  fitMeta();

  const vocabulary = $("#meta-vocabulary");
  if (vocabulary) headerSuggestions(meta, JSON.parse(vocabulary.textContent), data.newValue);

  // the editor opens at the line of the block chosen on the page; a new
  // page: in the header, after "title: "
  const cm = editor.codemirror;
  if (isNew) {
    const end = meta.value.indexOf("\n");
    meta.focus();
    meta.setSelectionRange(end < 0 ? meta.value.length : end, end < 0 ? meta.value.length : end);
  } else {
    const line = Math.max(0, Number(data.line) - 1);
    cm.setCursor({ line, ch: 0 });
    cm.scrollTo(null, cm.charCoords({ line, ch: 0 }, "local").top - 40);
    cm.focus();
  }

  // Files (frame .editor-files): those that the page links, and those sent
  // from this editor (to the folder of the page, see /new-file), each one
  // with a button that inserts its link at the cursor, and, for those sent,
  // a button that deletes it. A file dropped or pasted in the content is
  // also inserted there, one chosen with the button by its Insert button
  // only.
  const filesFrame = $("form.page-editor .editor-files");
  if (filesFrame) {
    const fd = filesFrame.dataset;
    const list = filesFrame.querySelector(".editor-files-list");
    const input = filesFrame.querySelector(".file-input");
    filesFrame.hidden = false;
    const pageQuery = isNew ? "" : `page=${encodeURIComponent(id)}`;
    const failure = async (response) => {
      if (response.status === 401) return data.errorSession;
      if (response.status === 403) return data.errorForbidden;
      if (response.status === 413) return fd.errorTooLarge;
      if (response.status === 400) return `${fd.errorRefused} ${await response.text()}`;
      if (response.status === 409) return await response.text();
      return `${data.errorFailed} (HTTP ${response.status})`;
    };
    const button = (className, text) => {
      const b = document.createElement("button");
      b.type = "button";
      b.className = className;
      b.textContent = text;
      return b;
    };

    // sends a file; its link goes to the bookmark, if any, after the text
    // before
    const send = async (file, mark, before) => {
      const row = document.createElement("li");
      row.className = "sending";
      const name = document.createElement("span");
      name.className = "file-name";
      name.textContent = file.name;
      row.append(name);
      list.append(row);
      const query = new URLSearchParams({ name: file.name });
      let response, reason;
      try {
        response = await fetch(`/new-file/${encodeURIComponent(source)}?${query}&${pageQuery}`,
                               { method: "POST", body: file });
        if (!response.ok) reason = await failure(response);
      } catch {
        reason = data.errorFailed;
      }
      if (reason) {
        row.remove();
        alert(`${file.name}\n${reason}`);
        return;
      }
      const { uid, name: served, link } = await response.json();
      sentFiles.set(uid, URL.createObjectURL(file));
      row.className = "sent";
      row.dataset.uid = uid;
      row.dataset.link = link;
      name.textContent = served;
      row.append(button("insert-file", fd.insert), button("delete-file", fd.delete));
      const at = mark?.find();
      if (at) cm.replaceRange(before + link, at);
    };
    // the files one after the other, their links in their order, a blank
    // line between them
    const sendAll = async (files, pos) => {
      const mark = pos ? cm.setBookmark(pos, { insertLeft: true }) : null;
      for (const [i, file] of files.entries()) await send(file, mark, i > 0 ? "\n\n" : "");
      mark?.clear();
    };

    filesFrame.querySelector(".attach-files").addEventListener("click", () => input.click());
    input.addEventListener("change", () => {
      sendAll([...input.files], null);
      input.value = "";
    });
    cm.on("drop", (_cm, e) => {
      const files = [...(e.dataTransfer?.files ?? [])];
      if (files.length === 0) return;
      e.preventDefault();
      sendAll(files, cm.coordsChar({ left: e.clientX, top: e.clientY }, "window"));
    });
    cm.on("paste", (_cm, e) => {
      const files = [...(e.clipboardData?.files ?? [])];
      if (files.length === 0) return;
      e.preventDefault();
      sendAll(files, cm.getCursor());
    });

    list.addEventListener("click", async (e) => {
      const b = e.target.closest("button");
      if (!b) return;
      const row = b.closest("li");
      if (b.classList.contains("insert-file")) {
        cm.replaceSelection(row.dataset.link);
        cm.focus();
      } else if (b.classList.contains("delete-file")) {
        b.disabled = true;
        const uid = row.dataset.uid;
        let reason;
        try {
          const response = await fetch(`/delete-file/${encodeURIComponent(source)}/${uid}?${pageQuery}`,
                                       { method: "POST" });
          if (!response.ok) reason = await failure(response);
        } catch {
          reason = data.errorFailed;
        }
        if (reason) {
          b.disabled = false;
          alert(reason);
          return;
        }
        URL.revokeObjectURL(sentFiles.get(uid));
        sentFiles.delete(uid);
        row.remove();
      }
    });
  }

  // leaving the page with changes asks for a confirmation
  let changed = false;
  cm.on("change", () => { changed = true; });
  meta.addEventListener("input", () => {
    changed = true;
    storage.set(meta.value);
    fitMeta();
  });
  addEventListener("beforeunload", (e) => { if (changed) e.preventDefault(); });

  const forgetDrafts = () => {
    editor.clearAutosavedValue();
    storage.clear();
  };

  // cancel: the drafts are forgotten
  $("form.page-editor .cancel").addEventListener("click", forgetDrafts);

  editorForm.addEventListener("submit", async (e) => {
    e.preventDefault();
    const save = $("form.page-editor .save");
    save.disabled = true;
    let response;
    try {
      // the fields of the form (header, message, path of a new page...),
      // and the body
      const params = new URLSearchParams(new FormData(editorForm));
      params.set("body", editor.value());
      response = await post(editorForm.getAttribute("action"), params);
    } catch {
      save.disabled = false;
      alert(data.errorFailed);
      return;
    }
    if (response.ok) {
      const url = await response.text();
      const ignored = response.headers.get("Kiwi-Ignored-Keys");
      if (ignored) alert(`${data.warningIgnored} ${ignored}`);
      forgetDrafts();
      changed = false;
      location.href = url;
      return;
    }
    save.disabled = false;
    if (response.status === 401) alert(data.errorSession);
    else if (response.status === 403) alert(data.errorForbidden);
    else if (response.status === 409) alert(data.errorConflict);
    else if (response.status === 400) alert(`${data.errorInvalid}\n${await response.text()}`);
    else alert(`${data.errorFailed} (HTTP ${response.status})`);
  });
}


// Copy buttons: their data-copy text, a tick shows it is done

document.addEventListener("click", async (e) => {
  const button = e.target.closest("button.copy");
  if (!button) return;
  await navigator.clipboard.writeText(button.dataset.copy);
  button.classList.add("copied");
  setTimeout(() => button.classList.remove("copied"), 1500);
});


// Images lightbox

const lightbox = document.createElement("dialog");
lightbox.className = "lightbox";
lightbox.addEventListener("click", () => lightbox.close());
document.body.append(lightbox);

function showImage(src) {
  const img = document.createElement("img");
  img.src = src;
  lightbox.replaceChildren(img);
  lightbox.showModal();
}

for (const img of $$(".document figure img")) {
  img.addEventListener("click", () => showImage(img.src));
}

for (const link of $$(".document a.kiwi-link-image")) {
  link.addEventListener("click", (e) => {
    e.preventDefault();
    showImage(link.href);
  });
}


// Table of contents : highlights the heading being read

const tocLinks = [...$$(".toc a")];
const headings = tocLinks.map((a) =>
  document.getElementById(a.getAttribute("href").slice(1)));

function highlightCurrentHeading() {
  // the last heading above the top of the window (below the navigation bar)
  const current = headings.findLastIndex((h) => h.getBoundingClientRect().top < 100);
  tocLinks.forEach((a, i) => a.classList.toggle("active", i === current));
}

if (tocLinks.length > 0) {
  addEventListener("scroll", highlightCurrentHeading, { passive: true });
  highlightCurrentHeading();
}
