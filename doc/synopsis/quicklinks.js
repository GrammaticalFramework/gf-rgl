// Find an element with a certain tag containing a certain text.
function findElement (tagname, text) {
  var els = document.body.getElementsByTagName(tagname)
  for (var i = 0; i < els.length; i++) {
    if (els[i].innerText === text) return els[i]
  }
  return null
}

function text (s) {
  return document.createTextNode(s)
}

function appendChildren (n, ds) {
  if (Array.isArray(ds)) for (var i in ds) n.appendChild(ds[i])
  else if (typeof ds === 'string') n.appendChild(text(ds))
  else n.appendChild(ds)
}

function node (tag, cls, ds) {
  var n = document.createElement(tag)
  if (cls) n.className = cls
  if (ds) appendChildren(n, ds)
  return n
}

function a (href, txt) {
  var a = node('a', '', txt)
  a.href = href
  return a
}

function forAllLinks (list, f) {
  for (var i = 0; i < list.length; i++) {
    var c = list[i].firstElementChild
    if (c && c.tagName === 'A' && c.href) f(c)
  }
}

/* -------------------------------------------------------------------------- */

// Extract links to the syntax rules
function listrules (ul) {
  var rules = []
  if (ul.tagName !== 'UL') return []
  forAllLinks(ul.children, function (c) {
    rules.push({
      href: c.href,
      text: c.innerText.split(' -')[0],
      full: c.innerText
    })
  })
  return rules
}

// Extract the links to the paradigm sections for all the languages
function listlangs (ul) {
  var langs = []
  if (ul.tagName !== 'UL') return []
  forAllLinks(ul.children, function (c) {
    if (/^Paradigms for /.test(c.innerText)) {
      langs.push({
        href: c.href,
        text: c.innerText.substr(14),
        full: c.innerText
      })
    }
  })
  return langs
}

function linklist (links) {
  var d = node('div')
  for (var i = 0; i < links.length; i++) {
    var l = a(links[i].href, links[i].text)
    l.title = links[i].full
    d.appendChild(l)
    d.appendChild(text(' '))
  }
  return d
}

function quicklinks () {
  // Find the detailed table of contents
  var h1toc = findElement('h1', 'Table of Contents')
  var ultoc = h1toc.nextElementSibling
  while (ultoc && ultoc.tagName !== 'UL') {
    ultoc = ultoc.nextElementSibling
  }

  var lis = ultoc.children

  var syntaxrules = []
  var langs = []

  // Find the Syntax Rules and Lexical Paradigms sections in the toc
  for (var i = 0; i < lis.length; i++) {
    var li = lis[i]
    var c = li.firstElementChild
    if (c.tagName === 'A') {
      if (/^Syntax Rules /.test(c.innerText)) {
        syntaxrules = listrules(c.nextElementSibling)
      } else if (c.innerText === 'Lexical Paradigms') {
        langs = listlangs(c.nextElementSibling)
      }
    }
  }

  return node(
    'div',
    'row',
    [ node('div', 'col-5', [ node('h6', '', 'Syntax'), linklist(syntaxrules) ]),
      node('div', 'col-7', [ node('h6', '', 'Morphology'), linklist(langs) ])
    ]
  )
}

appendChildren(
  document.getElementById('quicklinks'),
  [
    node('h5', '', 'Quick links'),
    quicklinks()
  ]
)

/* -------------------------------------------------------------------------- */

// Toggle quicklinks
function toggleSidebar () {
  var m = document.getElementById('main')
  var q = document.getElementById('quicklinks')
  var mClasses = 'col-md-9 col-xl-10'
  var qClasses = 'col-4 col-md-3 col-xl-2 offset-8 offset-md-9 offset-xl-10'
  if (hasClass(q, 'd-none')) {
    // Show
    addClass(m, mClasses)
    addClass(q, qClasses)
    removeClass(q, 'd-none')
  } else {
    // Hide
    removeClass(m, mClasses)
    removeClass(q, qClasses)
    addClass(q, 'd-none')
  }
}
document.getElementById('btn-quicklinks-toggle').addEventListener('click', toggleSidebar)
if (window.innerWidth >= 768) {
  // Below 768 (== 'md' breakpoint) sidebar overlays content and best left hidden
  toggleSidebar()
}

/* -------------------------------------------------------------------------- */

// Helpers

// Won't work on IE9; use className and split
function addClass (elem, classnames) {
  for (var c of classnames.split(' ')) elem.classList.add(c)
}
function removeClass (elem, classnames) {
  for (var c of classnames.split(' ')) elem.classList.remove(c)
}
// function toggleClass (elem, classnames) {
//   for (var c of classnames.split(' ')) elem.classList.toggle(c)
// }
function hasClass (elem, classname) {
  return elem.classList.contains(classname)
}
