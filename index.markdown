---
layout: root
---
This is the personal homepage of Felix Lange.  I like building software (using [Erlang](http://erlang.org),
[Ruby](http://ruby-lang.org) and [Racket](http://racket-lang.org)) as well as creating music with my friends.
I study computer science at the University of [Magdeburg, Germany][maps].
For matters most urgent, you may send electronic mail to
<span class="mail">fjl [at] twurst [dot] com</span>.

[maps]: http://maps.google.com/maps?f=q&amp;source=s_q&amp;hl=en&amp;q=Magdeburg,+Saxony-Anhalt,+Germany&amp;sll=37.09024,-95.712891&amp;sspn=32.197599,56.337891&amp;ie=UTF8&amp;cd=1&amp;geocode=FYx0GwMd3Y-xAA&amp;split=0&amp;hq=&amp;hnear=Magdeburg,+Saxony-Anhalt,+Germany&amp;ll=52.177721,11.636581&amp;spn=12.463064,28.168945&amp;t=h&amp;z=5

Blog Posts
==========
Also available as [atom](/feed/atom.xml)/[rss](/feed/rss.xml) feed.

<ul>
{% for p in site.posts %}
<li><span class="pdate">{{p.date | date: "%Y-%m-%d"}} &raquo;</span> <a href="{{ p.url }}">{{ p.title }}</a></li>
{% endfor %}
</ul>

Me, Elsewhere
=============
- [XING](https://www.xing.com/profile/Felix_Lange11), anyone?
- My [Stack Overflow account](http://stackoverflow.com/users/252612/felix-lange)
- I slash the web with [blekko](http://blekko.com/user/fjl)
- I'm [@fmakunbound](http://twitter.com/fmakunbound) on Twitter
- My [listening history](http://last.fm/user/polenkommutator) is on last.fm
- I host some of my projects [on GitHub](http://github.com/fjl)
- My [Google Profile](http://www.google.com/profiles/twursted) (not very interesting)

<div id="twitter">&nbsp;</div>
<div id="lastfm">&nbsp;</div>
<script type="text/javascript" src="http://ws.audioscrobbler.com/2.0/?method=user.recentTracks&amp;user=polenkommutator&amp;limit=10&amp;api_key=ca736a9da66216e778c1a909f827fd44&amp;format=json&amp;callback=Website.lastfm">
</script>
<script type="text/javascript" src="http://twitter.com/statuses/user_timeline/fmakunbound.json?count=10&amp;callback=Website.twitter">
</script>
