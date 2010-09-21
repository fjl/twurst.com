Website = (function () {
  function compLength(a, b) { return a.get('text').length - b.get('text').length; }
  function repeat(str, n) { return new Array(n + 1).join(str); }

  function markdownify() {
    // add =s under h1 headlines
    $$('h1').each(function (e) {
      if (!e.hasClass('_headlineMarkdownified')) {
        var tw = e.get('text').length;
        var c = e.get('html') + '<br/>' + repeat('=', tw);
        e.set('html', c);
        e.addClass('_headlineMarkdownified');
      }
    });

    // put * in front of li tags
    var liChar = '*'
    $$('li').each(function (e) {
      if (!e.hasClass('_listItemMarkdownified')) {
        var t = new Element('table', {styles: {padding: '0', 'border-collapse': 'collapse'}}).grab(
          new Element('tr', {styles: {padding: '0'}}).adopt([
            new Element('td', {html: liChar + '&nbsp;', styles: {padding: '0', 'vertical-align': 'top'}}),
            new Element('td', {html: e.get('html'), styles: {padding: '0'}})
          ])
        );
        e.empty()
        e.grab(t);
        e.setStyle('list-style-type', 'none');
        e.getParent().setStyle('padding-left', '0');
        e.addClass('_listItemMarkdownified');
      }
    });
  }

  // splits off sentence end character if present
  function wpart(str) {
    res = /(.*?)([.:?!])$/.exec(str)
    if (res != null)
      return [res[1], res[2]];
    else
      return [str, ""];
  }

  function formatStatus(str) {
    return str.split(' ').map(function (word) {
      w = wpart(word);
      if (word.test(/^http:\/\/.*/)) {
        return '<a href="' + word + '">' + word + '</a>';
      } else if (word.test(/^@.*/)) {
        return '<a href="http://twitter.com/' + w[0].substr(1) + '">' + word + '</a>' + w[1];
      } else if (word.test(/^#.*/)) {
        return '<a href="http://search.twitter.com/search?q=%23' + w[0].substr(1) + '">' + w[0] + '</a>' + w[1];
      } else {
        return word;
      }
    }).join(' ');
  }

  function displayTwitterStatuses(stats) {
    headline = new Element('h1', {html: 'Recent Twitter Statuses'});
    texts = new Element('ul');
    stats.each(function (s) {
      var li = new Element('li', {html: formatStatus(s.text)});
      li.inject(texts);
    });
    $('twitter').grab(headline);
    $('twitter').grab(texts);

    markdownify();
  }

  function init() {
    var sorted = $$('#accounts li').sort(compLength);
    $('accounts').adopt(sorted);

    $$('span.mail').each(function (e) {
      addr = e.get('text').replace(/ \[dot\] /, '.').replace(/ \[at\] /, '@');
      e.set('html', '<a href="mailto:' + addr + '">' + addr + '</a>');
    });

    markdownify();
  }

  window.addEvent('domready', init);

  return { twitter: displayTwitterStatuses };
})();
