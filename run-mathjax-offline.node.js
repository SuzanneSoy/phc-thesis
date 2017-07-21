// DO NOT run this on arbitrary unknown directory structures, as it evals the contents of the (user-provided) default.js configuration file.
const fs = require('fs');
const mjAPI = require('mathjax-node');
const jsdom = require('jsdom');

// Dynamically load the original MathJax config from …/MathJax/config/default.js
mjAPI.config({
  fontURL: 'MathJax/fonts/HTML-CSS/',
  MathJax: (function() {
    var cfg = {};
    var MathJax = {
      Hub: {
        Config: function(c) { cfg = c; }
      },
      Ajax: {
        loadComplete: function() { return; }
      }
    };
    cfgbytes = fs.readFileSync('doc/phc-thesis/MathJax/config/default.js');
    eval(cfgbytes + '');
    return cfg;
  })(),
});

jsdom.env('doc/phc-thesis/index.html', function(err, window) {
  var getmath = function () {
    var m = window.document.getElementsByClassName('math');
    var mm = [];
    for (var i = 0; i < m.length; i++) {
      mm[i] = m.item(i);
    }
    return mm;
  };
  var mm = getmath();
  var addedCSS = [];
  var processMathElement = function(i) {
    if (i >= mm.length) {
      process.stdout.write('\nSaving modified HTML file…\n');
      fs.writeFileSync('doc/phc-thesis/index2.html', jsdom.serializeDocument(window.document));
      return;
    } else {
      process.stdout.write((i+1)+'/'+mm.length+' '+(mm[i].tagName.toLowerCase() == 'div' ? 'display' : 'inline ')+'\r');
      mjAPI.typeset({
        math: mm[i].textContent,
        format: (mm[i].tagName.toLowerCase() == 'div' ? 'TeX' : 'inline-TeX'),
        htmlNode: true,
        css: true,
      }, function(result) {
        if (result.errors) {
          console.log(mm[i].textContent);
          process.exit(1);
        } else {
          // mark the initial element as hidden
          mm[i].classList.add('math-initial-hidden');

          // Add the CSS to the document
          if (addedCSS.indexOf(result.css) == -1){
            addedCSS.push(result.css);
            // Inject font-display: block; to prevent the browsers from stalling while applying fonts each time a new one is loaded.
            var css = result.css.split('@font-face {').join('@font-face {font-display: block; ');
            var styleElt = window.document.createElement('style');
            styleElt.setAttribute('type', 'text/css');
            var styleTxt = window.document.createTextNode(css);
            styleElt.appendChild(styleTxt);
            window.document.head.appendChild(styleElt);
          }
          
          // wrap the generated element in a class="MathJax_Preview" node
          preview = window.document.createElement((mm[i].tagName.toLowerCase() == 'div' ? 'div' : 'span'));
          preview.setAttribute('class', 'MathJax_Preview');
          preview.appendChild(result.htmlNode);
          mm[i].parentNode.insertBefore(preview, mm[i]);
          //console.log(result.htmlNode);
          processMathElement(i+1);
        }
      });
    }
  };
  // Remove the script which loads MathJax automatically, as it slows down things a lot when it re-renders the math.
  var scriptsInHead = window.document.head.getElementsByTagName('script');
  for (var i = 0; i < scriptsInHead.length; i++) {
    var e = scriptsInHead.item(i);
    if (e.innerHTML.trim() == decodeURI("(function()%20%7Bdocument.write('%3Cscr'%20+%20'ipt%20type=%22text/javascript%22%20src=%22MathJax/MathJax.js?config=default%22%3E%3C/scr'%20+%20'ipt%3E');%7D)();")) {
      e.parentNode.removeChild(e);
    }
  }
  // Fix the font size (mathjax-node cannot know easily the webpage's font size in advance).
  var patchFontSizeCode = "(function() { var outer = document.createElement('div'); outer.style.width = '0px'; outer.style.height = '0px'; outer.style.overflow = 'hidden'; var d = document.createElement('div'); /* 1.18rem from scribble's stylesheet, times 118% in the MathJax-generated CSS. */ outer.style.fontSize = (1.18 * 1.18) + 'rem'; d.style.width = '1000em'; outer.appendChild(d); document.body.appendChild(outer); window.setTimeout(function() { var sz = d.clientWidth / 1000; document.body.removeChild(outer); if (sz > 3) { var st = document.createElement('style'); st.appendChild(document.createTextNode('html .mjx-chtml { font-size: '+sz+'px; } html .mjx-chtml .mjx-chtml { font-size: inherit; }')); document.head.appendChild(st); } }, 0)})();";
  var patchFontSize = window.document.createElement('script');
  patchFontSize.appendChild(window.document.createTextNode(patchFontSizeCode));
  patchFontSize.setAttribute('type', 'text/javascript');
  window.document.body.insertBefore(patchFontSize, window.document.body.childNodes[0] || null);
  
  // set the initial <span class="math math-initial-hidden"></span> and <div …></div> elements hidden, as we manually injected a preview.
  hidestyle = window.document.createElement('style');
  hidestyle.setAttribute('type', 'text/css');
  hidestyle.appendChild(window.document.createTextNode('html .math-initial-hidden { display:none; visibility:hidden; } html .MathJax_Preview { color: inherit; }'));
  window.document.head.appendChild(hidestyle);
  processMathElement(0);
});
