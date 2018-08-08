"""
IPython Completions Server.

Copyright (C) 2013 Gerard Brunick

"""

from __future__ import print_function

import sys
from argparse import ArgumentParser
from functools import wraps

from SimpleXMLRPCServer import SimpleXMLRPCServer


client = None

# Method to send code to ipython
ipython_send = None
ipython_get_info = None
ipython_execute_silently = None
ipython_complete = None

def show_traceback(f):
    @wraps(f)
    def wrapper(*args):
        try:
            return f(*args)
        except:
            import traceback
            traceback.print_exc()
            raise
    return wrapper

@show_traceback
def complete(line, pos=None):
    print("\ncomplete: %r %r" % (line, pos))
    pos = pos or len(line)
    begin = pos
    while begin > 0 and line[begin-1] not in " \t":
        begin -= 1
    print("\ncomplete: %r %r %r" % (line[begin:pos], line, pos))
    matches = ipython_complete(line[begin:pos], line, pos)
    print("matches: %r" % matches)
    return matches


@show_traceback
def execute_code(code):
    """Execute code silently in IPython."""
    print("\nexecute_code: %r" % code)
    if ipython_execute_silently is not None:
        result = ipython_execute_silently(code)
        print("\nResult: %s\n " % result)
        return result
    print("Error: no connection")
    return None


@show_traceback
def get_info(symbol):
    print("\nget_info: %r" % symbol)
    return ipython_get_info(symbol)

@show_traceback
def close():
    sys.exit()

def get_shell_channel(connection_file):
    try:
        from IPython.lib.kernel import find_connection_file
        from IPython.zmq.blockingkernelmanager import BlockingKernelManager
        cf = find_connection_file(connection_file)
        km = BlockingKernelManager(connection_file=cf)
        km.load_connection_file()
        km.start_channels()
        return km.shell_channel
    except ImportError:
        import IPython.kernel as kernel
        f = kernel.find_connection_file(connection_file)
        manager = kernel.KernelManager(connection_file=f)
        manager.load_connection_file()
        client = manager.client()
        client.start_channels()
        return client.shell_channel

def old_setup():
    global client, ipython_send, ipython_complete, ipython_get_info
    global ipython_execute_silently

    from IPython.lib.kernel import find_connection_file
    from IPython.zmq.blockingkernelmanager import BlockingKernelManager
    cf = find_connection_file("") #args.connection_file)
    km = BlockingKernelManager(connection_file=cf)
    km.load_connection_file()
    km.start_channels()
    shell = km.shell_channel
    io = km.sub_channel

    def complete(word, line, pos):
        msg_id = shell.complete(word, line, pos)
        reply = shell.get_msg()
        assert reply['parent_header']['msg_id'] == msg_id
        status = reply['content']['status']
        if status == 'ok':
            matches = reply['content']['matches']
            # print "matches: %r" % matches
            return matches
        elif status == 'error':
            tb = reply['content']['traceback']
            print("Error: %s" % tb)
            return tb
    ipython_complete = complete



def new_setup():
    global client, ipython_send, ipython_complete, ipython_get_info
    global ipython_execute_silently

    import IPython.kernel as kernel
    f = kernel.find_connection_file("")
    manager = kernel.KernelManager(connection_file=f)
    manager.load_connection_file()
    client = manager.client()
    client.start_channels()

    try:
        ipython_send = client.shell_channel.execute
    except AttributeError:
        # API change
        ipython_send = client.execute

    def complete(word, line, pos):
        try:
            client.complete(word, line, pos)
        except TypeError:
            # API change
            client.complete(line, pos)
        msg = client.get_shell_msg()
        header = msg["header"]
        print("Waiting for 'complete_reply' message ...", end=' ')
        while header["msg_type"] != 'complete_reply':
            msg = client.get_shell_msg()
            header = msg["header"]
        print('done')
        content = msg["content"]
        assert(header["msg_type"] == 'complete_reply')
        assert(content['status'] == 'ok')
        return content["matches"]
    ipython_complete = complete

    def get_info(object_name):
        msg_id = client.inspect(object_name)
        print(msg_id)
        msg_type = 'inspect_reply'

        msg = client.get_shell_msg()
        header = msg["header"]
        assert header["msg_type"] == 'execute_reply'

        msg = client.get_shell_msg()
        print(msg)
        header = msg["header"]
        parent_header = msg["parent_header"]
        content = msg["content"]
        assert(header["msg_type"] == msg_type)
        assert(parent_header["msg_id"] == msg_id)
        return content['data']
    ipython_get_info = get_info

    def exec_silent(code):
        msg_id = client.execute(code, True, allow_stdin=False)
        print(msg_id)
        msg = client.get_shell_msg()
        while msg['parent_header']['msg_id'] != msg_id:
            msg = client.get_shell_msg()
        return msg
    ipython_execute_silently = exec_silent


@show_traceback
def connect(connect_info=""):
    print("connect(%r)..." % connect_info)
    try:
        import IPython.kernel as kernel
        new_setup()
    except:
        import IPython.lib.kernel as kernel
        old_setup()
    print("Connected.")


@show_traceback
def debug():
    from IPython import embed
    embed()


if __name__ == '__main__':
    # parser = ArgumentParser()
    # parser.add_argument('connection_file', type=str)
    # args = parser.parse_args()

    # Load connection info and connect to ipython kernel
    # shell, io = get_shell_channel(args.connection_file)

    # Start up XML-RPC server to provide completion information.
    server = SimpleXMLRPCServer(("localhost", 0), allow_none=True)
    print("Serving ipython IPC requests to http://%s:%s" % server.server_address)
    sys.stdout.flush()
    server.register_function(connect)
    server.register_function(complete)
    server.register_function(execute_code, "execute-code")
    server.register_function(get_info, "get-info")
    server.register_function(debug)
    server.register_function(close)
    server.serve_forever()

