"""
Server Python completions using jedi.

Copyright (C) 2013 Gerard Brunick

"""

import sys
from argparse import ArgumentParser
from functools import wraps

from SimpleXMLRPCServer import SimpleXMLRPCServer

import jedi
jedi.settings.case_insensitive_completion = False
# jedi.settings.add_dot_after_module = True
# jedi.settings.add_bracket_after_function = True

buffers = {}

debug = True

def jedi_script(source, line, column, source_path):
    # Not sure why the following is necessary?
    if (sys.version_info[0] >= 3):
        source = source.encode('utf-8')
        source_path = source_path and source_path.encode('utf-8')
    return jedi.Script(source, line, column, source_path or '')


def show_traceback(f):
    @wraps(f)
    def wrapper(*args):
        try:
            return f(*args)
        except:
            import traceback
            msg = traceback.format_exc()
            #traceback.print_exc()
            print msg
            from os.path import abspath, splitext
            logfile_name, ext = splitext(abspath(__file__))
            logfile_name += ".log"
            with open(logfile_name, "at") as log:
                log.write("\n%s%r\n" % (f.__name__, args))
                log.write(msg)
                if f.__name__ in ["complete", "get_eldoc_message"]:
                    try:
                        log.write("buffer = %r\n\n" % (buffers[args[0]],))
                    except KeyError:
                        pass
                print "Wrote debug information to %s" % logfile_name
            raise
    return wrapper


@show_traceback
def close():
    sys.exit()


@show_traceback
def register_buffer(name, content):
    if debug:
        print "\nregister_buffer(%r, ...)" % (name) #, content[:100])
    buffers[name] = content
    return name


@show_traceback
def update_buffers(changes):
    if debug:
        print "\nupdate_buffers..." % (changes,)
    _update_buffers(changes)


def _update_buffers(changes):
    changes.sort()
    if debug:
        print "_update_buffers:"
        for i, buf_name, beg, end, new_text in changes:
            print "%6i %s[%i:%i]=%r" % (i, buf_name, beg, end, new_text)
    for i, buf_name, beg, end, new_text in changes:
        text = buffers[buf_name]
        buffers[buf_name] = text[:beg] + new_text + text[end:]


@show_traceback
def rename_buffer(old_name, new_name):
    if debug:
        print "\nrename_buffer: %r -> %r" % (old_name, new_name)
    buffers[new_name] = buffers[old_name]
    del buffers[old_name]
    return new_name


@show_traceback
def get_buffer_contents(name):
    if debug:
        print "\nget_buffer_contents(%r)" % (name,)
        # print "buffers: %r" % (buffers.keys())
    return buffers[name]


@show_traceback
def complete(buffer_name, line, column, source_path, updates):
    if debug:
        print "\ncomplete(%r, %r, %r, %r, ...)" % (buffer_name, line,
                                                  column, source_path)
    if updates:
        _update_buffers(updates)
    source = buffers[buffer_name]
    script = jedi_script(source, line, column, source_path)
    completions = script.completions()
    def make_name(c):
        # print "type: %r" % c.type
        if c.type == 'function':
            return c.name + "("
        return c.name
    result = [make_name(c) for c in completions]
    output = result
    if len(output) > 10:
        output = result[:5] + ["..."] + result[-5:]
    print "result: %r" % (output,)
    return result


@show_traceback
def get_eldoc_message(buffer_name, line, column, source_path, updates):
    if debug:
        msg = "\nget_eldoc_info(%r, %r, %r, %r, ...)"
        print msg % (buffer_name, line, column, source_path)
    if updates:
        _update_buffers(updates)
    source = buffers[buffer_name]
    script = jedi_script(source, line, column, source_path)
    call_signatures = script.call_signatures()
    print "call_signatures: %r" % (call_signatures,)
    if call_signatures:
        cs = call_signatures[0]
        result = [cs.call_name, cs.index]
        result += [str(p.get_name()) for p in cs.params]
        print "result: %r" % result
        # from IPython import embed; embed()
        return result
    else:
        print "result: None"
        return []

def test_method():
    print "Test method..."
    return []

if __name__ == '__main__':
    # Start up XML-RPC server to provide completion information.
    server = SimpleXMLRPCServer(("localhost", 0), allow_none=True)
    print "Serving python information to http://%s:%s" % server.server_address
    sys.stdout.flush()
    server.register_function(test_method, "test-method")
    server.register_function(register_buffer, "register-buffer")
    server.register_function(update_buffers, "update-buffers")
    server.register_function(rename_buffer, "rename-buffer")
    server.register_function(get_buffer_contents, "get-buffer-contents")
    server.register_function(complete)
    server.register_function(get_eldoc_message)
    server.register_function(close)
    server.serve_forever()
