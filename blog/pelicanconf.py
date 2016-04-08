#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

AUTHOR = 'Mauricio Vargas S. 帕夏'
SITENAME = 'Reimagined Invention'
SITESUBTITLE = 'Blog about LaTeX, R and Statistics'
SITEURL = 'http://pachamaltese.github.io'
GOOGLE_ANALYTICS = 'UA-65764144-1'
AUTHOR_GITHUB = 'pachamaltese'
AUTHOR_TWITTER = 'pachamaltese'
AUTHOR_LINKEDIN = 'mvargassepulveda'
AUTHOR_EMAIL = 'mauriciovargas@ug.uchile.cl'
AUTHOR_SLIDESHARE = 'pachamaltese'
SITE_DESCRIPTION = 'Mauricio Vargas S. (@pachamaltese): Pachá 帕夏 ～ Data Science 科学数据专业化 (R 编程语言) ～ Trade Policy 国际贸易。I worked for @lafundacionsol and I was a @wtochairchile scholar。'
	
PATH = 'content'

TIMEZONE = 'Asia/Shanghai'

DEFAULT_LANG = u'en'
DEFAULT_DATE = 'fs'

# Feed generation is usually not desired when developing
FEED_ALL_ATOM = None
CATEGORY_FEED_ATOM = None
TRANSLATION_FEED_ATOM = None
AUTHOR_FEED_ATOM = None
AUTHOR_FEED_RSS = None

DEFAULT_PAGINATION = 5

# Uncomment following line if you want document-relative URLs when developing
RELATIVE_URLS = True

THEME = "/Users/pacha/pachamaltese.github.io/blog/theme" 
COLOR_SCHEME_CSS = 'github.css'

PLUGIN_PATHS = ['/Users/pacha/pachamaltese.github.io/blog/pelican-plugins']
PLUGINS = ['rmd_reader', 'render_math']

