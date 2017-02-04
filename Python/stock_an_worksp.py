{
	"auto_complete":
	{
		"selected_items":
		[
			[
				"time",
				"timestamp	statement"
			],
			[
				"fina",
				"findAll	function"
			],
			[
				"beau",
				"BeautifulSoup	class"
			],
			[
				"urlli",
				"urllib2"
			],
			[
				"url",
				"urllib2"
			],
			[
				"append",
				"appendLine	statement"
			],
			[
				"cand",
				"candleArray	statement"
			],
			[
				"sl",
				"sleep"
			],
			[
				"save",
				"saveFile	statement"
			],
			[
				"split",
				"splitLine	statement"
			],
			[
				"Split",
				"splitSource	statement"
			],
			[
				"uri",
				"urlToVisit	statement"
			],
			[
				"from",
				"fromtimestamp	function"
			],
			[
				"date",
				"datetime	class"
			],
			[
				"sto",
				"stockToPull	statement"
			],
			[
				"file",
				"fileLine	statement"
			],
			[
				"r",
				"random	module"
			],
			[
				"ap",
				"append"
			],
			[
				"p",
				"print"
			],
			[
				"top",
				"top1000"
			],
			[
				"grou",
				"groupby"
			],
			[
				"rat",
				"rating_std_by_title"
			],
			[
				"so",
				"sorted_by_diff"
			],
			[
				"me",
				"mean_ratings"
			],
			[
				"mean",
				"mean_ratings"
			],
			[
				"ra",
				"rating"
			],
			[
				"merge",
				"merge_2"
			],
			[
				"mer",
				"merge_1"
			],
			[
				"ratings",
				"ratings"
			],
			[
				"count",
				"count_subset"
			],
			[
				"value_",
				"value_counts"
			],
			[
				"va",
				"value_counts"
			],
			[
				"counts",
				"counts"
			],
			[
				"c",
				"counts"
			],
			[
				"v",
				"value_key_pairs"
			]
		]
	},
	"buffers":
	[
		{
			"file": "get_weather_data.py",
			"settings":
			{
				"buffer_size": 1640,
				"line_ending": "Windows"
			}
		},
		{
			"file": "stock_an_v3.py",
			"settings":
			{
				"buffer_size": 1334,
				"line_ending": "Windows"
			}
		},
		{
			"file": "stock_an_v4.py",
			"settings":
			{
				"buffer_size": 1444,
				"line_ending": "Windows"
			}
		},
		{
			"contents": "from distutils.core import setup\n\ntry:\n    from distutils.command.build_py import build_py_2to3 as build_py\nexcept ImportError:\n    # 2.x\n    from distutils.command.build_py import build_py\n\nsetup(name=\"beautifulsoup4\",\n      version = \"4.3.2\",\n      author=\"Leonard Richardson\",\n      author_email='leonardr@segfault.org',\n      url=\"http://www.crummy.com/software/BeautifulSoup/bs4/\",\n      download_url = \"http://www.crummy.com/software/BeautifulSoup/bs4/download/\",\n      long_description=\"\"\"Beautiful Soup sits atop an HTML or XML parser, providing Pythonic idioms for iterating, searching, and modifying the parse tree.\"\"\",\n      license=\"MIT\",\n      packages=['bs4', 'bs4.builder', 'bs4.tests'],\n      cmdclass = {'build_py':build_py},\n      classifiers=[\"Development Status :: 4 - Beta\",\n                   \"Intended Audience :: Developers\",\n                   \"License :: OSI Approved :: MIT License\",\n                   \"Programming Language :: Python\",\n                   'Programming Language :: Python :: 3',\n                   \"Topic :: Text Processing :: Markup :: HTML\",\n                   \"Topic :: Text Processing :: Markup :: XML\",\n                   \"Topic :: Text Processing :: Markup :: SGML\",\n                   \"Topic :: Software Development :: Libraries :: Python Modules\",\n                   ],\n)\n",
			"file": "/C/Python27/dist/bs/setup.py",
			"file_size": -1,
			"file_write_time": -1,
			"settings":
			{
				"buffer_size": 1323,
				"line_ending": "Unix"
			}
		},
		{
			"settings":
			{
				"buffer_size": 0,
				"line_ending": "Windows"
			}
		},
		{
			"settings":
			{
				"buffer_size": 0,
				"line_ending": "Windows"
			}
		},
		{
			"settings":
			{
				"buffer_size": 0,
				"line_ending": "Windows"
			}
		},
		{
			"contents": "\"\"\"Diagnostic functions, mainly for use when doing tech support.\"\"\"\nimport cProfile\nfrom StringIO import StringIO\nfrom HTMLParser import HTMLParser\nimport bs4\nfrom bs4 import BeautifulSoup, __version__\nfrom bs4.builder import builder_registry\n\nimport os\nimport pstats\nimport random\nimport tempfile\nimport time\nimport traceback\nimport sys\nimport cProfile\n\ndef diagnose(data):\n    \"\"\"Diagnostic suite for isolating common problems.\"\"\"\n    print \"Diagnostic running on Beautiful Soup %s\" % __version__\n    print \"Python version %s\" % sys.version\n\n    basic_parsers = [\"html.parser\", \"html5lib\", \"lxml\"]\n    for name in basic_parsers:\n        for builder in builder_registry.builders:\n            if name in builder.features:\n                break\n        else:\n            basic_parsers.remove(name)\n            print (\n                \"I noticed that %s is not installed. Installing it may help.\" %\n                name)\n\n    if 'lxml' in basic_parsers:\n        basic_parsers.append([\"lxml\", \"xml\"])\n        from lxml import etree\n        print \"Found lxml version %s\" % \".\".join(map(str,etree.LXML_VERSION))\n\n    if 'html5lib' in basic_parsers:\n        import html5lib\n        print \"Found html5lib version %s\" % html5lib.__version__\n\n    if hasattr(data, 'read'):\n        data = data.read()\n    elif os.path.exists(data):\n        print '\"%s\" looks like a filename. Reading data from the file.' % data\n        data = open(data).read()\n    elif data.startswith(\"http:\") or data.startswith(\"https:\"):\n        print '\"%s\" looks like a URL. Beautiful Soup is not an HTTP client.' % data\n        print \"You need to use some other library to get the document behind the URL, and feed that document to Beautiful Soup.\"\n        return\n    print\n\n    for parser in basic_parsers:\n        print \"Trying to parse your markup with %s\" % parser\n        success = False\n        try:\n            soup = BeautifulSoup(data, parser)\n            success = True\n        except Exception, e:\n            print \"%s could not parse the markup.\" % parser\n            traceback.print_exc()\n        if success:\n            print \"Here's what %s did with the markup:\" % parser\n            print soup.prettify()\n\n        print \"-\" * 80\n\ndef lxml_trace(data, html=True, **kwargs):\n    \"\"\"Print out the lxml events that occur during parsing.\n\n    This lets you see how lxml parses a document when no Beautiful\n    Soup code is running.\n    \"\"\"\n    from lxml import etree\n    for event, element in etree.iterparse(StringIO(data), html=html, **kwargs):\n        print(\"%s, %4s, %s\" % (event, element.tag, element.text))\n\nclass AnnouncingParser(HTMLParser):\n    \"\"\"Announces HTMLParser parse events, without doing anything else.\"\"\"\n\n    def _p(self, s):\n        print(s)\n\n    def handle_starttag(self, name, attrs):\n        self._p(\"%s START\" % name)\n\n    def handle_endtag(self, name):\n        self._p(\"%s END\" % name)\n\n    def handle_data(self, data):\n        self._p(\"%s DATA\" % data)\n\n    def handle_charref(self, name):\n        self._p(\"%s CHARREF\" % name)\n\n    def handle_entityref(self, name):\n        self._p(\"%s ENTITYREF\" % name)\n\n    def handle_comment(self, data):\n        self._p(\"%s COMMENT\" % data)\n\n    def handle_decl(self, data):\n        self._p(\"%s DECL\" % data)\n\n    def unknown_decl(self, data):\n        self._p(\"%s UNKNOWN-DECL\" % data)\n\n    def handle_pi(self, data):\n        self._p(\"%s PI\" % data)\n\ndef htmlparser_trace(data):\n    \"\"\"Print out the HTMLParser events that occur during parsing.\n\n    This lets you see how HTMLParser parses a document when no\n    Beautiful Soup code is running.\n    \"\"\"\n    parser = AnnouncingParser()\n    parser.feed(data)\n\n_vowels = \"aeiou\"\n_consonants = \"bcdfghjklmnpqrstvwxyz\"\n\ndef rword(length=5):\n    \"Generate a random word-like string.\"\n    s = ''\n    for i in range(length):\n        if i % 2 == 0:\n            t = _consonants\n        else:\n            t = _vowels\n        s += random.choice(t)\n    return s\n\ndef rsentence(length=4):\n    \"Generate a random sentence-like string.\"\n    return \" \".join(rword(random.randint(4,9)) for i in range(length))\n        \ndef rdoc(num_elements=1000):\n    \"\"\"Randomly generate an invalid HTML document.\"\"\"\n    tag_names = ['p', 'div', 'span', 'i', 'b', 'script', 'table']\n    elements = []\n    for i in range(num_elements):\n        choice = random.randint(0,3)\n        if choice == 0:\n            # New tag.\n            tag_name = random.choice(tag_names)\n            elements.append(\"<%s>\" % tag_name)\n        elif choice == 1:\n            elements.append(rsentence(random.randint(1,4)))\n        elif choice == 2:\n            # Close a tag.\n            tag_name = random.choice(tag_names)\n            elements.append(\"</%s>\" % tag_name)\n    return \"<html>\" + \"\\n\".join(elements) + \"</html>\"\n\ndef benchmark_parsers(num_elements=100000):\n    \"\"\"Very basic head-to-head performance benchmark.\"\"\"\n    print \"Comparative parser benchmark on Beautiful Soup %s\" % __version__\n    data = rdoc(num_elements)\n    print \"Generated a large invalid HTML document (%d bytes).\" % len(data)\n    \n    for parser in [\"lxml\", [\"lxml\", \"html\"], \"html5lib\", \"html.parser\"]:\n        success = False\n        try:\n            a = time.time()\n            soup = BeautifulSoup(data, parser)\n            b = time.time()\n            success = True\n        except Exception, e:\n            print \"%s could not parse the markup.\" % parser\n            traceback.print_exc()\n        if success:\n            print \"BS4+%s parsed the markup in %.2fs.\" % (parser, b-a)\n\n    from lxml import etree\n    a = time.time()\n    etree.HTML(data)\n    b = time.time()\n    print \"Raw lxml parsed the markup in %.2fs.\" % (b-a)\n\n    import html5lib\n    parser = html5lib.HTMLParser()\n    a = time.time()\n    parser.parse(data)\n    b = time.time()\n    print \"Raw html5lib parsed the markup in %.2fs.\" % (b-a)\n\ndef profile(num_elements=100000, parser=\"lxml\"):\n\n    filehandle = tempfile.NamedTemporaryFile()\n    filename = filehandle.name\n\n    data = rdoc(num_elements)\n    vars = dict(bs4=bs4, data=data, parser=parser)\n    cProfile.runctx('bs4.BeautifulSoup(data, parser)' , vars, vars, filename)\n\n    stats = pstats.Stats(filename)\n    # stats.strip_dirs()\n    stats.sort_stats(\"cumulative\")\n    stats.print_stats('_html5lib|bs4', 50)\n\nif __name__ == '__main__':\n    diagnose(sys.stdin.read())\n",
			"file": "/C/Python27/Lib/site-packages/bs4/diagnose.py",
			"file_size": -1,
			"file_write_time": -1,
			"settings":
			{
				"buffer_size": 6315,
				"line_ending": "Unix"
			}
		},
		{
			"file": "try1.py",
			"settings":
			{
				"buffer_size": 1434,
				"line_ending": "Windows"
			}
		},
		{
			"contents": "Package Control Messages\n========================\n\nSublimerge Pro:\n--------------\n\n  Sublimerge has just been installed. Thanks!\n  \n  It is highly recommended to restart Sublime Text before first use.\n  ------------------------------------------------------------------\n  \n  Simplified usage instructions\n  =============================\n  \n  In file view:\n  \n      [ctrl]+[alt]+[d] - display Quick Panel with Sublimerge commands available for current view\n  \n  In diff view:\n  \n      [ctrl]+[alt]+[d] - display Differences Navigator\n      [ctrl]+[alt]+[=] - select the next difference (only single selection is possible)\n      [ctrl]+[alt]+[-] - select the previous difference (only single selection is possible)\n      [ctrl]+[alt]+[,] - merge selected change(s) from right to left\n      [ctrl]+[alt]+[.] - merge selected change(s) from left to right\n      [ctrl]+[alt]+[/] + [,] - merge all changes from right to left\n      [ctrl]+[alt]+[/] + [.] - merge all changes from left to right\n      [ctrl]+[alt]+[i] - toggle edit mode (currently in 2-way diff view only)\n      [ctrl]+[alt]+[left click] - select/deselect change block (multiple selection is possible)\n      [ctrl]+[shift]+[left click] - merge selected changes from left to right\n      [ctrl]+[shift]+[right click] - merge selected changes from right to left\n  \n  In directories diff view:\n  \n      [ctrl]+[alt]+[d] - enter subdirectory (if differs) or diff selected text file\n      [ctrl]+[alt]+[=] - select next file/directory\n      [ctrl]+[alt]+[-] - select previous file/directory or navigate one level up\n      [ctrl]+[alt]+[left click] - select file/directory\n      [ctrl]+[alt]+[.] - apply selected change from left to right\n      [ctrl]+[alt]+[/]+[.] - apply all changes from left to right\n      [ctrl]+[alt]+[,] - apply selected change from right to left\n      [ctrl]+[alt]+[/]+[,] - apply all changes from right to left\n  \n  \n  Licensing\n  =========\n  \n  If you like Sublimerge, please support development by buying a license. It doesn't cost much! :)\n  \n  \n  Solving most common problems\n  ============================\n  \n  Sublimerge doesn't work. Console reports: \"ImportError: No module named 'sublimerge'\"\n  \n  If you installed Sublimerge using Package Control, please make sure that you use its final version.\n  Alpha does not install Sublimerge properly. You can also try Manual Installation (please view\n  sublimerge.com/installation.html for more details)\n  \n  \n  See also\n  ========\n  \n  If you are using Gerrit Code Review, please take a look at SublimeGerrit - the full featured\n  Gerrit Code Review integration for Sublime Text - http://www.sublimegerrit.com/\n  \n  \n  More information\n  ================\n  \n  For more information, troubleshooting, customizing settings and more, please visit www.sublimerge.com\n  \n",
			"settings":
			{
				"buffer_size": 2796,
				"line_ending": "Windows",
				"name": "Package Control Messages",
				"scratch": true
			}
		},
		{
			"file": "stock_an_v2.py",
			"settings":
			{
				"buffer_size": 1838,
				"line_ending": "Windows"
			}
		},
		{
			"file": "stock_an.py",
			"settings":
			{
				"buffer_size": 1227,
				"line_ending": "Windows"
			}
		},
		{
			"file": "stock_starter.py",
			"settings":
			{
				"buffer_size": 698,
				"line_ending": "Windows"
			}
		}
	],
	"build_system": "",
	"command_palette":
	{
		"height": 173.0,
		"selected_items":
		[
			[
				"py",
				"Set Syntax: Python"
			],
			[
				"pyth",
				"Set Syntax: Python"
			],
			[
				"package",
				"Package Control: Enable Package"
			],
			[
				"install",
				"Package Control: Install Package"
			],
			[
				"di",
				"Set Syntax: Diff"
			],
			[
				"enable",
				"Package Control: Enable Package"
			],
			[
				"pack",
				"Package Control: Enable Package"
			],
			[
				"default",
				"Preferences: Package Control Settings – Default"
			],
			[
				"disable",
				"Package Control: Disable Package"
			],
			[
				"reset",
				"Preferences: Settings - Default"
			],
			[
				"dis",
				"Package Control: Disable Package"
			],
			[
				"disa",
				"Package Control: Disable Package"
			],
			[
				"pac",
				"Package Control: Disable Package"
			],
			[
				"instal",
				"Package Control: Install Package"
			]
		],
		"width": 400.0
	},
	"console":
	{
		"height": 160.0,
		"history":
		[
			"import urllib.request,os,hashlib; h = '7183a2d3e96f11eeadd761d777e62404' + 'e330c659d4bb41d3bdf022e94cab3cd0'; pf = 'Package Control.sublime-package'; ipp = sublime.installed_packages_path(); urllib.request.install_opener( urllib.request.build_opener( urllib.request.ProxyHandler()) ); by = urllib.request.urlopen( 'http://sublime.wbond.net/' + pf.replace(' ', '%20')).read(); dh = hashlib.sha256(by).hexdigest(); print('Error validating download (got %s instead of %s), please try manual install' % (dh, h)) if dh != h else open(os.path.join( ipp, pf), 'wb' ).write(by)"
		]
	},
	"distraction_free":
	{
		"menu_visible": true,
		"show_minimap": false,
		"show_open_files": true,
		"show_tabs": false,
		"side_bar_visible": false,
		"status_bar_visible": false
	},
	"file_history":
	[
		"/C/Users/Jeffrey/Google Drive/Code/oneDayOHLC/stock_an7_vBlackedOut_2.py",
		"/C/Users/Jeffrey/Google Drive/Code/oneDayOHLC/stock_an7_vBlackedOut.py",
		"/C/Users/Jeffrey/Google Drive/Code/oneDayOHLC/stock_an7_vCandle.py",
		"/C/Users/Jeffrey/Google Drive/Code/prime.py",
		"/C/Users/Jeffrey/Google Drive/Code/reverse.py",
		"/C/Users/Jeffrey/Google Drive/Code/wunder_data.txt",
		"/C/Users/Jeffrey/Google Drive/Code/AAPL.txt",
		"/C/Users/Jeffrey/Google Drive/Code/py/battleship",
		"/C/Users/Jeffrey/AppData/Roaming/Sublime Text 3/Packages/User/Default (Windows).sublime-keymap",
		"/C/Users/Jeffrey/Google Drive/Code/stock_an_v4.py",
		"/C/Users/Jeffrey/Google Drive/Code/oneDayOHLC/TSLA.txt",
		"/C/Users/Jeffrey/Google Drive/Code/CMG.txt",
		"/C/Users/Jeffrey/Google Drive/Code/GOOG.txt",
		"/C/Users/Jeffrey/AppData/Roaming/Sublime Text 3/Packages/Package Control/Package Control.sublime-settings",
		"/C/Users/Jeffrey/AppData/Roaming/Sublime Text 3/Packages/Default/Preferences.sublime-settings",
		"/C/Users/Jeffrey/AppData/Roaming/Sublime Text 3/Packages/Default/Default (Windows).sublime-keymap",
		"/C/Users/Jeffrey/Google Drive/Code/stock_an_v2.py",
		"/C/Users/Jeffrey/Google Drive/Code/while_else.py",
		"/C/Users/Jeffrey/Google Drive/Code/MSFT.txt",
		"/C/Users/Jeffrey/Google Drive/Code/NFLX.txt",
		"/C/Users/Jeffrey/Google Drive/Code/AMZN.txt",
		"/C/Users/Jeffrey/Google Drive/Code/ch2_names_eg.py",
		"/C/Users/Jeffrey/Google Drive/Code/prices_stocks.py",
		"/C/Users/Jeffrey/Google Drive/Code/outliers_base.py",
		"/C/Users/Jeffrey/Google Drive/Code/gpa.py",
		"/C/Users/Jeffrey/Google Drive/Code/battleship",
		"/C/Users/Jeffrey/Google Drive/Code/oneDayOHLC/AAPL.txt",
		"/C/Users/Jeffrey/Google Drive/Code/ch3_egs.py",
		"/C/Users/Jeffrey/Google Drive/Code/ch2_movie_eg.py",
		"/C/Users/Jeffrey/Google Drive/Code/ch2_baby_eg.py"
	],
	"find":
	{
		"height": 38.0
	},
	"find_in_files":
	{
		"height": 0.0,
		"where_history":
		[
		]
	},
	"find_state":
	{
		"case_sensitive": false,
		"find_history":
		[
			"5998ff",
			"		\n",
			"ax1",
			"axes",
			"subplot",
			"int",
			"ctrl+k",
			"lastUnix = 0 #if the file above doesn't exist, take the whole thing (we want it)\n			",
			"int",
			"# if turn == 3:\n            #     print \"Game Over\"",
			"if turn == 3:\n                print \"Game Over\"",
			"board",
			"random",
			"ship_row",
			"average",
			"numbers"
		],
		"highlight": true,
		"in_selection": false,
		"preserve_case": false,
		"regex": false,
		"replace_history":
		[
		],
		"reverse": false,
		"show_context": true,
		"use_buffer2": true,
		"whole_word": false,
		"wrap": true
	},
	"groups":
	[
		{
			"selected": 0,
			"sheets":
			[
				{
					"buffer": 0,
					"file": "get_weather_data.py",
					"semi_transient": false,
					"settings":
					{
						"buffer_size": 1640,
						"regions":
						{
						},
						"selection":
						[
							[
								812,
								812
							]
						],
						"settings":
						{
							"syntax": "Packages/Python/Python.tmLanguage"
						},
						"translation.x": -0.0,
						"translation.y": 288.0,
						"zoom_level": 1.0
					},
					"stack_index": 0,
					"type": "text"
				}
			]
		},
		{
			"selected": 6,
			"sheets":
			[
				{
					"buffer": 1,
					"file": "stock_an_v3.py",
					"semi_transient": false,
					"settings":
					{
						"buffer_size": 1334,
						"regions":
						{
						},
						"selection":
						[
							[
								321,
								321
							]
						],
						"settings":
						{
							"syntax": "Packages/Python/Python.tmLanguage"
						},
						"translation.x": 0.0,
						"translation.y": 0.0,
						"zoom_level": 1.0
					},
					"stack_index": 9,
					"type": "text"
				},
				{
					"buffer": 2,
					"file": "stock_an_v4.py",
					"semi_transient": false,
					"settings":
					{
						"buffer_size": 1444,
						"regions":
						{
						},
						"selection":
						[
							[
								323,
								323
							]
						],
						"settings":
						{
							"syntax": "Packages/Python/Python.tmLanguage",
							"translate_tabs_to_spaces": false
						},
						"translation.x": 0.0,
						"translation.y": 0.0,
						"zoom_level": 1.0
					},
					"stack_index": 6,
					"type": "text"
				},
				{
					"buffer": 3,
					"file": "/C/Python27/dist/bs/setup.py",
					"semi_transient": false,
					"settings":
					{
						"buffer_size": 1323,
						"regions":
						{
						},
						"selection":
						[
							[
								0,
								0
							]
						],
						"settings":
						{
							"syntax": "Packages/Python/Python.tmLanguage"
						},
						"translation.x": 0.0,
						"translation.y": 0.0,
						"zoom_level": 1.0
					},
					"stack_index": 5,
					"type": "text"
				},
				{
					"buffer": 4,
					"semi_transient": false,
					"settings":
					{
						"buffer_size": 0,
						"regions":
						{
						},
						"selection":
						[
							[
								0,
								0
							]
						],
						"settings":
						{
							"syntax": "Packages/Text/Plain text.tmLanguage"
						},
						"translation.x": 0.0,
						"translation.y": 0.0,
						"zoom_level": 1.0
					},
					"stack_index": 4,
					"type": "text"
				},
				{
					"buffer": 5,
					"semi_transient": false,
					"settings":
					{
						"buffer_size": 0,
						"regions":
						{
						},
						"selection":
						[
							[
								0,
								0
							]
						],
						"settings":
						{
							"syntax": "Packages/Text/Plain text.tmLanguage"
						},
						"translation.x": 0.0,
						"translation.y": 0.0,
						"zoom_level": 1.0
					},
					"stack_index": 3,
					"type": "text"
				},
				{
					"buffer": 6,
					"semi_transient": false,
					"settings":
					{
						"buffer_size": 0,
						"regions":
						{
						},
						"selection":
						[
							[
								0,
								0
							]
						],
						"settings":
						{
							"syntax": "Packages/Text/Plain text.tmLanguage"
						},
						"translation.x": 0.0,
						"translation.y": 0.0,
						"zoom_level": 1.0
					},
					"stack_index": 2,
					"type": "text"
				},
				{
					"buffer": 7,
					"file": "/C/Python27/Lib/site-packages/bs4/diagnose.py",
					"semi_transient": false,
					"settings":
					{
						"buffer_size": 6315,
						"regions":
						{
						},
						"selection":
						[
							[
								0,
								0
							]
						],
						"settings":
						{
							"syntax": "Packages/Python/Python.tmLanguage"
						},
						"translation.x": 0.0,
						"translation.y": 0.0,
						"zoom_level": 1.0
					},
					"stack_index": 1,
					"type": "text"
				},
				{
					"buffer": 8,
					"file": "try1.py",
					"semi_transient": false,
					"settings":
					{
						"buffer_size": 1434,
						"regions":
						{
						},
						"selection":
						[
							[
								831,
								831
							]
						],
						"settings":
						{
							"syntax": "Packages/Python/Python.tmLanguage",
							"translate_tabs_to_spaces": false
						},
						"translation.x": 0.0,
						"translation.y": 384.0,
						"zoom_level": 1.0
					},
					"stack_index": 7,
					"type": "text"
				},
				{
					"buffer": 9,
					"semi_transient": false,
					"settings":
					{
						"buffer_size": 2796,
						"regions":
						{
						},
						"selection":
						[
							[
								2796,
								2796
							]
						],
						"settings":
						{
							"syntax": "Packages/Text/Plain text.tmLanguage",
							"word_wrap": true
						},
						"translation.x": 0.0,
						"translation.y": 0.0,
						"zoom_level": 1.0
					},
					"stack_index": 8,
					"type": "text"
				},
				{
					"buffer": 10,
					"file": "stock_an_v2.py",
					"semi_transient": false,
					"settings":
					{
						"buffer_size": 1838,
						"regions":
						{
						},
						"selection":
						[
							[
								848,
								848
							]
						],
						"settings":
						{
							"syntax": "Packages/Python/Python.tmLanguage",
							"translate_tabs_to_spaces": false
						},
						"translation.x": 0.0,
						"translation.y": 0.0,
						"zoom_level": 1.0
					},
					"stack_index": 10,
					"type": "text"
				},
				{
					"buffer": 11,
					"file": "stock_an.py",
					"semi_transient": false,
					"settings":
					{
						"buffer_size": 1227,
						"regions":
						{
						},
						"selection":
						[
							[
								371,
								444
							]
						],
						"settings":
						{
							"syntax": "Packages/Python/Python.tmLanguage",
							"translate_tabs_to_spaces": false
						},
						"translation.x": 227.0,
						"translation.y": 0.0,
						"zoom_level": 1.0
					},
					"stack_index": 11,
					"type": "text"
				},
				{
					"buffer": 12,
					"file": "stock_starter.py",
					"semi_transient": false,
					"settings":
					{
						"buffer_size": 698,
						"regions":
						{
						},
						"selection":
						[
							[
								0,
								0
							]
						],
						"settings":
						{
							"syntax": "Packages/Python/Python.tmLanguage"
						},
						"translation.x": 0.0,
						"translation.y": 0.0,
						"zoom_level": 1.0
					},
					"stack_index": 12,
					"type": "text"
				}
			]
		}
	],
	"incremental_find":
	{
		"height": 30.0
	},
	"input":
	{
		"height": 0.0
	},
	"layout":
	{
		"cells":
		[
			[
				0,
				0,
				1,
				1
			],
			[
				1,
				0,
				2,
				1
			]
		],
		"cols":
		[
			0.0,
			0.973958333333,
			1.0
		],
		"rows":
		[
			0.0,
			1.0
		]
	},
	"menu_visible": true,
	"output.exec":
	{
		"height": 34.0
	},
	"output.find_results":
	{
		"height": 0.0
	},
	"project": "stock_an.sublime-project",
	"replace":
	{
		"height": 56.0
	},
	"save_all_on_build": true,
	"select_file":
	{
		"height": 0.0,
		"selected_items":
		[
		],
		"width": 0.0
	},
	"select_project":
	{
		"height": 500.0,
		"selected_items":
		[
		],
		"width": 380.0
	},
	"select_symbol":
	{
		"height": 0.0,
		"selected_items":
		[
		],
		"width": 0.0
	},
	"settings":
	{
	},
	"show_minimap": true,
	"show_open_files": false,
	"show_tabs": true,
	"side_bar_visible": false,
	"side_bar_width": 233.0,
	"status_bar_visible": true,
	"template_settings":
	{
	}
}
