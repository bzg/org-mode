# Python utilities for ob-python.el
#
# Copyright (C) 2026 Free Software Foundation, Inc.
#
# This file is part of GNU Emacs.
#
# GNU Emacs is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

import ast
import pprint

def session_results_output(
        src_file_name, context
):
    with open(src_file_name) as f:
        exec(compile(f.read(), f.name, 'exec'), context)

def session_results_value(
        src_file_name, result_file_name, result_params, context
):
    with open(src_file_name) as f:
        src_ast = ast.parse(f.read())

    final_line = src_ast.body[-1]

    if isinstance(final_line, ast.Expr):
        src_ast.body = src_ast.body[:-1]
        exec(compile(src_ast, '<string>', 'exec'), context)
        final_line = eval(compile(ast.Expression(
            final_line.value), '<string>', 'eval'), context)
    else:
        exec(compile(src_ast, '<string>', 'exec'), context)
        final_line = None

    format_results_value(
        final_line,
        result_file_name,
        result_params
    )

def clear_graphics():
    import matplotlib.pyplot
    matplotlib.pyplot.gcf().clear()

def save_graphics(file_name):
    import matplotlib.pyplot
    matplotlib.pyplot.savefig(file_name)

def format_results_value(result, result_file_name, result_params):
    with open(result_file_name, 'w') as result_file_obj:
        if 'graphics' in result_params:
            result.savefig(result_file_name)
        elif 'pp' in result_params:
            result_file_obj.write(pprint.pformat(result))
        elif 'list' in result_params and isinstance(result, dict):
            result_file_obj.write(
                str(['{} :: {}'.format(k, v) for k, v in result.items()])
            )
        else:
            if not set(result_params).intersection(['scalar', 'verbatim', 'raw']):
                def dict2table(res):
                    if isinstance(res, dict):
                        return [(k, dict2table(v)) for k, v in res.items()]
                    elif isinstance(res, list) or isinstance(res, tuple):
                        return [dict2table(x) for x in res]
                    else:
                        return res
                if 'table' in result_params:
                    result = dict2table(result)
                try:
                    import pandas
                except ImportError:
                    pass
                else:
                    if isinstance(result, pandas.DataFrame) and 'table' in result_params:
                        result = [
                            [result.index.name or ''] + list(result.columns)
                        ] + [None] + [
                            [i] + list(row) for i, row in result.iterrows()
                        ]
                    elif isinstance(result, pandas.Series) and 'table' in result_params:
                        result = list(result.items())
                try:
                    import numpy
                except ImportError:
                    pass
                else:
                    if isinstance(result, numpy.ndarray):
                        if 'table' in result_params:
                            result = result.tolist()
                        else:
                            result = repr(result)
            result_file_obj.write(str(result))
