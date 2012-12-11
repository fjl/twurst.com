---
layout: root
---
This is the personal homepage of Felix Lange.
I like building software (using [Common Lisp][hyperspec], [Erlang][erlang], [Racket][racket]),
cooking and music. 
I live in Berlin, Germany. In case you want to contact me, send electronic mail to
<span class="mail">fjl [at] twurst [dot] com</span>.

I currently work for *Veodin Software* building [KeyRocket](http://keyrocket.com/),
a smart keyboard shortcut trainer for Microsoft Windows™.

[hyperspec]: http://www.ai.mit.edu/projects/iiip/doc/CommonLISP/HyperSpec/FrontMatter/index.html "Common Lisp Hyperspec"
[erlang]: http://erlang.org "Erlang Homepage"
[racket]: http://racket-lang.org "Racket Homepage"

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
<ul>
  <li><a rel="me" href="https://www.xing.com/profile/Felix_Lange11">XING</a>, anyone?</li>
  <li>My <a href='http://stackoverflow.com/users/252612/felix-lange' rel='me'>Stack Overflow account</a></li>
  <li>I slash the web with <a href='http://blekko.com/user/fjl' rel='me'>blekko</a></li>
  <li>I&#8217;m <a href='http://twitter.com/fmakunbound' rel='me'>@fmakunbound</a> on Twitter</li>
  <li>My <a href='http://last.fm/user/polenkommutator' rel='me'>listening history</a> is on last.fm</li>
  <li>I host some of my software projects <a href='http://github.com/fjl' rel='me'>on GitHub</a></li>
  <li>My <a href='http://www.google.com/profiles/twursted' rel='me'>Google Profile</a> (not very interesting)</li>
</ul>

<div id="lastfm">&nbsp;</div>
<script type="text/javascript" src="http://ws.audioscrobbler.com/2.0/?method=user.recentTracks&amp;user=polenkommutator&amp;limit=10&amp;api_key=ca736a9da66216e778c1a909f827fd44&amp;format=json&amp;callback=Website.lastfm">
</script>
