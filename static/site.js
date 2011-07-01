Website = (function () {
  var dashWidth = 8;
  var liChar = '*&nbsp;'

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

  function markdownifyH1(e) {
    if (!e.hasClass('_headlineMarkdownified')) {
      var tw = Math.min(Math.round(e.getWidth() / dashWidth), e.get("text").length);
      var c = e.get('html') + '<br/>' + repeat('=', tw);
      e.set('html', c);
      e.addClass('_headlineMarkdownified');
    }
    return e;
  }

  function markdownify(el) {
    // add =s under h1 headlines
    $$('h1').each(markdownifyH1);

    // put * in front of li tags
    $$('ul').each(function (e) {
      var t = new Element('table');
      var items = e.children;
      for (var i = 0; i < items.length; i++) {
        new Element('tr').adopt([
          new Element('td', {html: liChar}),
          new Element('td', {html: items.item(i).get('html')})
        ]).inject(t);
      }
      t.replaces(e);
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

  function displayTwitterStatuses(data) {
    var headline = new Element('h1', {id: 'twitter-headline', text: 'Recent Twitter Statuses'});
    var list = new Element('table').adopt(
      data.map(function (status) {
        return new Element('tr').adopt([
          new Element('td', {html: liChar}),
          new Element('td', {html: formatStatus(status.text)})
        ]);
      })
    );
    document.id('twitter').empty().adopt([headline, list]);
    markdownifyH1($('twitter-headline'));
  }

  function displayLastfmTracks(data) {
    if (data.recenttracks != undefined) {
      var headline = new Element('h1', {id: 'lastfm-headline', text: 'Recent Last.fm Tracks'});
      var list = new Element('table');
      data.recenttracks.track.forEach(function (t) {
        var bullet = liChar;
        if (t['@attr'] && t['@attr'].nowplaying && t['@attr'].nowplaying == 'true') {
          bullet = '&#9654;&nbsp;';
        }
        var artist = t.artist['#text'];
        var artistLink = 'http://last.fm/music/' + encodeURIComponent(t.artist['#text']);
        new Element('tr').adopt([
          new Element('td', {html: bullet}),
          new Element('td').grab(new Element('a', {href: artistLink, html: artist})),
          new Element('td', {html: '&nbsp;&mdash;&nbsp;'}),
          new Element('td', {html: t.name})
        ]).inject(list);
      });
      document.id('lastfm').empty().adopt([headline, list]);
      markdownifyH1($('lastfm-headline'));
    }
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
    var ruleText = new Element('span', {id: 'dashwidthHack', text: '-', styles: {position: 'absolute', left: '-3000px'}});
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

  return { rm: markdownifyHR, twitter: displayTwitterStatuses, lastfm: displayLastfmTracks };
})();
