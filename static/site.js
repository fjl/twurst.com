Website = (function () {
  var dashWidth = 8;

  function compLength(a, b) { return a.get('text').length - b.get('text').length; }
  function repeat(str, n) { return new Array(n + 1).join(str); }

  function markdownifyHR() {
    $$('hr').each(function (hr) {
      new Element('p', {'class': '_ruleMarkdownified'}).replaces(hr);
    });
    $$('p._ruleMarkdownified').each(function (el) {
      el.set('html', repeat('-', Math.floor(el.getWidth() / dashWidth)));
    });
  }

  function markdownify() {
    // add =s under h1 headlines
    $$('h1').each(function (e) {
      if (!e.hasClass('_headlineMarkdownified')) {
        var tw = Math.min(Math.round(e.getWidth() / dashWidth), e.get("text").length);
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

    markdownifyHR();
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

  function resize() {
    dashWidth = $('dashwidthHack').getWidth();
    markdownifyHR();
  }

  // ripped off http://stackoverflow.com/questions/995914/catch-browsers-zoom-event-in-javascript/3596295#3596295
  // Poll the pixel width of the window; invoke resize() if the width has been changed.
  var lastWidth = 0;
  function pollZoomFireEvent() {
    var widthNow = $(window).getWidth();
    if (lastWidth == widthNow) return;
    lastWidth = widthNow;
    // width changed, user must have zoomed.
    resize();
  }

  function init() {
    var ruleText = new Element('span', {id: 'dashwidthHack', html: '-', styles: {position: 'absolute', left: '-3000px'}});
    $$('body').adopt(ruleText);
    dashWidth = ruleText.getWidth();

    $$('span.mail').each(function (e) {
      addr = e.get('text').replace(/ \[dot\] /, '.').replace(/ \[at\] /, '@');
      e.set('html', '<a href="mailto:' + addr + '">' + addr + '</a>');
    });

    markdownify();
    setInterval(pollZoomFireEvent, 100);
  }

  window.addEvent('domready', init);
  window.addEvent('resize', resize);

  return { rm: markdownifyHR, twitter: displayTwitterStatuses };
})();
