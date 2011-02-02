---
layout: post
title: Using Chyrp for a small blog site
---
My father, Guido Lange, has started his own personal blog at
[postwurf.net][postwurf]. "Postwurf" means bulk mail in german.
He will share his thoughts about life,
society and the fall of print media. I did the setup and layout,
using the [Chyrp blogging engine][chyrp] by Alex Suraci. Chyrp might
not have as many plugins as Wordpress, but it is a great choice for
small personal blogs and websites. I'll outline some of it's
features below.

Chyrp has a [git repository][chyrp-git] and the master branch appears
to be pretty stable. The setup page is nice and does not ask a lot of
questions other than the usual ones about database and admin
credentials. It is notable that Chyrp supports MySQL and SQLite
(PostgreSQL will be supported in the future). After that, the site
runs with the default theme and plugins installed.

I decided not to go with the default theme, but to adapt it.
Chyrp feels more like a small web framework with content-management
functionality than a blogging engine. The templating engine is based
on [Twig][twig], which is similar to the [Django Template Language][DTL].
Chyrp's data model is clean and properly documented, so changing the templates was very easy.
GNU gettext is used for localization.

Plugins are divided into *modules*, which add general functionality like
entry tagging and *feathers*, which allow entry formats other than just
text (i.e. embedded video) to be posted.
Coming from [Serendipity][s9y], I really missed an automated plugin installation facility.
Currently one has to download a zip archive and install manually through the file system.

One really neat feature is inline editing. Entries, comments and pages
can be edited and deleted though an AJAX-powered dialogue
frame. Comment submission also works without a reload.

Overall, chyrp is a consistent and easy-to-use system you really should
take into consideration when setting up a personal website.

[postwurf]: http://postwurf.net "postwurf.net"
[chyrp]:  http://chyrp.net/ "chyrp homepage"
[chyrp-git]: http://github.com/vito/chyrp.git "chyrp git repository"
[twig]: http://www.twig-project.org/ "twig homepage"
[DTL]: http://docs.djangoproject.com/en/dev/topics/templates/ "Django Template Language documentation"
[s9y]: http://s9y.org "Serendipity homepage"
