#! /usr/bin/env python

from pygments import highlight
from pygments.util import ClassNotFound
from pygments.lexers import get_lexer_by_name, TextLexer
from pygments.formatters import HtmlFormatter

from pygments.lexer import RegexLexer
from pygments.token import *

import sys

def main(program_name, args):
	if len(args) != 2:
		sys.stderr.write("Usage: %s <language> <tab-size>\n" % program_name)
		sys.exit(0)

	language = args[0]
	tab_size = int(args[1])

	if language == 'cks_type':
		lexer = CksTypeLexer(tabsize=tab_size)
	elif language == 'cks_value':
		lexer = CksValueLexer(tabsize=tab_size)
	else:
		try:
			lexer = get_lexer_by_name(language, stripall=True, tabsize=tab_size)
		except ClassNotFound:
			lexer = TextLexer()

	formatter = HtmlFormatter(linenos=False, cssclass="highlight")

	code = sys.stdin.read()
	sys.stdout.write(highlight(code, lexer, formatter))

class CksTypeLexer(RegexLexer):
	name = 'CKS type lexer'

	tokens = {
		'root': [
			(r'[^/]+', Text),
			(r'/\*', Comment.Multiline, 'comment'),
			(r'//.*?$', Comment.Single),
			(r'/', Text),
		],
		'comment': [
			(r'\*/', Comment.Multiline),
			(r'.', Comment.Multiline),
		]
	}

class CksValueLexer(RegexLexer):
	name = 'CKS value lexer'

	tokens = {
		'root': [
			(r'/\*', Comment.Multiline, 'comment'),
			(r'//.*?$', Comment.Single),
         (r'0x[0-9a-f_]+', Number),
         (r'[0-9_]+', Number.Integer),
			(r'"(\\\\|\\"|[^"])*"', String),
			(r'.', Text),
		],
		'comment': [
			(r'[^*/]', Comment.Multiline),
			(r'[*/]', Comment.Multiline),
		]
	}

if __name__ == '__main__':
	main(sys.argv[0], sys.argv[1:])

