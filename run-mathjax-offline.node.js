// DO NOT run this on arbitrary unknown directory structures, as it evals the contents of the (user-provided) default.js configuration file.
const fs = require('fs');
const mjAPI = require('mathjax-node');
const jsdom = require('jsdom');

// Dynamically load the original MathJax config from …/MathJax/config/default.js
mjAPI.config({ MathJax: (function() {
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
})(), });

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
  var process = function(i) {
    if (i >= mm.length) {
      fs.writeFileSync('doc/phc-thesis/index2.html', jsdom.serializeDocument(window.document));
      return;
    } else {
      mjAPI.typeset({
        math: mm[i].innerHTML,
        format: 'inline-TeX',
        htmlNode: true,
        css: true,
      }, function(result) {
        if (!(result.errors)) {
          // mark the initial element as hidden
          mm[i].classList.add('math-initial-hidden');

          // Add the CSS to the document
          var css = window.document.createElement('style');
          css.setAttribute('type', 'text/css');
          var csstxt = window.document.createTextNode(result.css);
          css.appendChild(csstxt);
          window.document.head.appendChild(css);
          
          // wrap the generated element in a class="MathJax_Preview" node
          preview = window.document.createElement(result.tagName);
          preview.setAttribute('class', 'MathJax_Preview');
          preview.appendChild(result.htmlNode);
          mm[i].parentNode.insertBefore(preview, mm[i]);
          //console.log(result.htmlNode);
          process(i+1);
        }
      });
    }
  };
  // Fix the font size (mathjax-node cannot know easily the webpage's font size in advance).
  var patchFontSizeCode = "(function() { var d = document.createElement('div'); d.style.width = '2260ex'; document.body.appendChild(d); window.setTimeout(function() { var sz = d.clientWidth / 1000; document.body.removeChild(d); if (sz > 3) { var st = document.createElement('style'); st.appendChild(document.createTextNode('.mjx-chtml { font-size: '+sz+'px; } .mjx-chtml .mjx-chtml {font-size: inherit }')); document.head.appendChild(st); } }, 0)})();";
  var patchFontSize = window.document.createElement('script');
  patchFontSize.appendChild(window.document.createTextNode(patchFontSizeCode));
  patchFontSize.setAttribute('type', 'text/javascript');
  window.document.body.insertBefore(patchFontSize, window.document.body.childNodes[0] || null);
  
  // set the initial <span class="math math-initial-hidden"></span> and <div …></div> elements hidden, as we manually injected a preview.
  hidestyle = window.document.createElement('style');
  hidestyle.setAttribute('type', 'text/css');
  hidestyle.appendChild(window.document.createTextNode('.math-initial-hidden { display:none; visibility:hidden; } .MathJax_Preview { color: inherit; }'));
  window.document.head.appendChild(hidestyle);
  process(0);
});
