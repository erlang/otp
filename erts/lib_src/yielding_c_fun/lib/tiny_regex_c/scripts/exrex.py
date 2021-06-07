#!/usr/bin/env python

# This file is part of exrex.
#
# exrex is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# exrex is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with exrex. If not, see < http://www.gnu.org/licenses/ >.
#
# (C) 2012- by Adam Tauber, <asciimoo@gmail.com>

try:
    from future_builtins import map, range
except:
    pass
from re import sre_parse
from itertools import product, chain, tee
from random import choice,randint
import string

__all__ = ('generate', 'CATEGORIES', 'count', 'parse', 'getone')

CATEGORIES = {'category_space'  : sorted(sre_parse.WHITESPACE)
             ,'category_digit'  : sorted(sre_parse.DIGITS)
             ,'category_any'    : [chr(x) for x in range(32, 123)]
             ,'category_word'   : sorted( frozenset(string.ascii_letters + string.digits + "_") )
             }

def comb(g, i):
    for c in g:
        g2,i = tee(i)
        for c2 in g2:
            yield c+c2

def mappend(g, c):
    for cc in g:
        yield cc+c

def _in(d):
    ret = []
    neg = False
    for i in d:
        if i[0] == 'range':
            subs = map(chr, range(i[1][0], i[1][1]+1))
            if neg:
                for char in subs:
                    try:
                        ret.remove(char)
                    except:
                        pass
            else:
                ret.extend(subs)
        elif i[0] == 'literal':
            if neg:
                try:
                    ret.remove(chr(i[1]))
                except:
                    pass
            else:
                ret.append(chr(i[1]))
        elif i[0] == 'category':
            subs = CATEGORIES.get(i[1], [''])
            if neg:
                for char in subs:
                    try:
                        ret.remove(char)
                    except:
                        pass
            else:
                ret.extend(subs)
        elif i[0] == 'negate':
            ret = list(CATEGORIES['category_any'])
            neg = True
    return ret


def prods(orig, ran, items):
    for o in orig:
        for r in ran:
            for s in product(items, repeat=r):
                yield o+''.join(s)

def ggen(g1, f, *args, **kwargs):
    for a in g1:
        g2 = f(*args, **kwargs)
        if isinstance(g2, int):
            yield g2
        else:
            for b in g2:
                yield a+b

def _gen(d, limit=20, count=False):
    """docstring for _gen"""
    ret = ['']
    strings = 0
    for i in d:
        if i[0] == 'in':
            subs = _in(i[1])
            if count:
                strings = (strings or 1) * len(subs)
            ret = comb(ret, subs)
        elif i[0] == 'literal':
            ret = mappend(ret, chr(i[1]))
        elif i[0] == 'category':
            subs = CATEGORIES.get(i[1], [''])
            if count:
                strings = (strings or 1) * len(subs)
            ret = comb(ret, subs)
        elif i[0] == 'any':
            subs = CATEGORIES['category_any']
            if count:
                strings = (strings or 1) * len(subs)
            ret = comb(ret, subs)
        elif i[0] == 'max_repeat':
            chars = filter(None, _gen(list(i[1][2]), limit))
            if i[1][1]+1 - i[1][0] >= limit:
                ran = range(i[1][0], i[1][0]+limit)
            else:
                ran = range(i[1][0], i[1][1]+1)
            if count:
                for i in ran:
                    strings += pow(len(chars), i)
            ret = prods(ret, ran, chars)
        elif i[0] == 'branch':
            subs = list(chain.from_iterable(_gen(list(x), limit) for x in i[1][1]))
            if count:
                strings = (strings or 1) * (len(subs) or 1)
            ret = comb(ret, subs)
        elif i[0] == 'subpattern':
            if count:
                strings = (strings or 1) * (sum(ggen([0], _gen, i[1][1], limit=limit, count=True)) or 1)
            ret = ggen(ret, _gen, i[1][1], limit=limit, count=False)
        # ignore ^ and $
        elif i[0] == 'at':
            continue
        elif i[0] == 'not_literal':
            subs = list(CATEGORIES['category_any'])
            subs.remove(chr(i[1]))
            if count:
                strings = (strings or 1) * len(subs)
            ret = comb(ret, subs)
        elif i[0] == 'assert':
            print i[1][1]
            continue
        else:
            #print('[!] cannot handle expression ' + repr(i))
            raise Exception('[!] cannot handle expression ' + repr(i))

    if count:
        return strings

    return ret

def _randone(d, limit=20):
    """docstring for _randone"""
    ret = ''
    for i in d:
        if i[0] == 'in':
            ret += choice(_in(i[1]))
        elif i[0] == 'literal':
            ret += chr(i[1])
        elif i[0] == 'category':
            ret += choice(CATEGORIES.get(i[1], ['']))
        elif i[0] == 'any':
            ret += choice(CATEGORIES['category_any'])
        elif i[0] == 'max_repeat':
            chars = filter(None, _gen(list(i[1][2]), limit))
            if i[1][1]+1 - i[1][0] >= limit:
                min,max = i[1][0], i[1][0]+limit
            else:
                min,max = i[1][0], i[1][1]
            for _ in range(randint(min, max)):
                ret += choice(chars)
        elif i[0] == 'branch':
            ret += choice(list(chain.from_iterable(_gen(list(x), limit) for x in i[1][1])))
        elif i[0] == 'subpattern':
            ret += _randone(i[1][1], limit)
        elif i[0] == 'at':
            continue
        elif i[0] == 'not_literal':
            c=list(CATEGORIES['category_any'])
            c.remove(chr(i[1]))
            ret += choice(c)
        else:
            #print('[!] cannot handle expression "%s"' % str(i))
            raise Exception('[!] cannot handle expression "%s"' % str(i))

    return ret


def parse(s):
    """Regular expression parser
    :param s: Regular expression
    :type s: str
    :rtype: list
    """
    r = sre_parse.parse(s)
    return list(r)

def generate(s, limit=20):
    """Creates a generator that generates all matching strings to a given regular expression
    :param s: Regular expression
    :type s: str
    :param limit: Range limit
    :type limit: int
    :returns: string generator object
    """
    return _gen(parse(s), limit)

def count(s, limit=20):
    """Counts all matching strings to a given regular expression
    :param s: Regular expression
    :type s: str
    :param limit: Range limit
    :type limit: int
    :rtype: int
    :returns: number of matching strings
    """
    return _gen(parse(s), limit, count=True)

def getone(regex_string, limit=20):
    """Returns a random matching string to a given regular expression
    """
    return _randone(parse(regex_string), limit)

def argparser():
    import argparse
    from sys import stdout
    argp = argparse.ArgumentParser(description='exrex - regular expression string generator')
    argp.add_argument('-o', '--output'
                     ,help      = 'Output file - default is STDOUT'
                     ,metavar   = 'FILE'
                     ,default   = stdout
                     ,type      = argparse.FileType('w')
                     )
    argp.add_argument('-l', '--limit'
                     ,help      = 'Max limit for range size - default is 20'
                     ,default   = 20
                     ,action    = 'store'
                     ,type      = int
                     ,metavar   = 'N'
                     )
    argp.add_argument('-c', '--count'
                     ,help      = 'Count matching strings'
                     ,default   = False
                     ,action    = 'store_true'
                     )
    argp.add_argument('-r', '--random'
                     ,help      = 'Returns a random string that matches to the regex'
                     ,default   = False
                     ,action    = 'store_true'
                     )
    argp.add_argument('-d', '--delimiter'
                     ,help      = 'Delimiter - default is \\n'
                     ,default   = '\n'
                     )
    argp.add_argument('-v', '--verbose'
                     ,action    = 'store_true'
                     ,help      = 'Verbose mode'
                     ,default   = False
                     )
    argp.add_argument('regex'
                     ,metavar   = 'REGEX'
                     ,help      = 'REGEX string'
                     )
    return vars(argp.parse_args())

def __main__():
    from sys import exit, stderr
    # 'as(d|f)qw(e|r|s)[a-zA-Z]{2,3}'
    # 'as(QWE|Z([XC]|Y|U)V){2,3}asdf'
    # '.?'
    # '.+'
    # 'asdf.{1,4}qwer{2,5}'
    # 'a(b)?(c)?(d)?'
    # 'a[b][c][d]?[e]?
    args = argparser()
    if args['verbose']:
        args['output'].write('%r%s' % (parse(args['regex'], limit=args['limit']), args['delimiter']))
    if args['count']:
        args['output'].write('%d%s' % (count(args['regex'], limit=args['limit']), args['delimiter']))
        exit(0)
    if args['random']:
        args['output'].write('%s%s' % (getone(args['regex'], limit=args['limit']), args['delimiter']))
        exit(0)
    try:
        g = generate(args['regex'], args['limit'])
    except Exception, e:
        print >> stderr, '[!] Error: ', e
        exit(1)
    for s in g:
        try:
            args['output'].write(s+args['delimiter'])
        except:
            break

if __name__ == '__main__':
    __main__()

