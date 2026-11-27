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

  // the preview is rendered by kiwi, as the page would be; the side by side
  // preview is rendered again once the typing stops
  let previewTimer;
  const renderPreview = (text, preview) => {
    clearTimeout(previewTimer);
    previewTimer = setTimeout(async () => {
      try {
        const response = await post("/preview", { source, id, body: text });
        preview.innerHTML = await response.text();
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
