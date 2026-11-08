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

$(".top-nav .reload")?.addEventListener("click", (e) =>
  adminAction(e.currentTarget, "/reload"));

// absent without editor
$(".top-nav .edit-page")?.addEventListener("click", (e) => {
  const { pageSource, pageId } = e.currentTarget.dataset;
  adminAction(e.currentTarget, "/edit-page",
              { "page-source": pageSource, "page-id": pageId });
});


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
