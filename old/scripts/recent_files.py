"""Desktop integration code.

Designed to be cross platform.
"""

"Determine the desktop environment"

import os
import sys
import urllib, urlparse
from os.path import abspath, basename, exists, splitext

platform = sys.platform

if platform in ["linux2", "linux2-ubuntu"]:
    desktop_type = os.environ.get("DESKTOP_SESSION")
    if desktop_type:
        platform = "%s-%s" % (platform, desktop_type)

extension_map = {
    ".pdf" : "application/pdf",
    ".ps" : "application/postscript",
    ".djvu" : "image/vnd.djvu",
    ".txt" : "text/plain"
}


try:
    import magic
except ImportError:
    import mimetypes
    def get_mime_type(filename):
        return mimetypes.guess_type(filename)[0]
        # base, ext = splitext(filename)
        # return extension_map[ext]
else:
    try:
        mime_type_getter = magic.Magic(mime=True)
    except TypeError:
        mime_type_getter = magic.open(magic.MAGIC_MIME)
        mime_type_getter.load()
        def get_mime_type(filename):
            return mime_type_getter.file(filename)
    else:
        def get_mime_type(filename):
            return mime_type_getter.from_file(filename)

if platform in ['linux2-gnome', "linux2", "linux2-ubuntu"]:
    import gtk
    recent_manager = None
    def get_manager():
        global recent_manager
        if recent_manager != None:
            return recent_manager
        recent_manager = gtk.recent_manager_get_default()
        recent_manager = gtk.RecentManager()
        return recent_manager

    def add_recent_file(filename, mime_type=None, app_name=None, app_exec=None):
        # base, ext = splitext(filename)
        if mime_type == None:
            mime_type = get_mime_type(filename)
            if mime_type == None: #except KeyError:
                msg = "I don't know the mime type info for the file %s."
                raise RuntimeError(msg % basename(filename))
            else:
                return "mime type:", mime_type

        filename = abspath(filename)
        if exists(filename):
            url = "file://" + urllib.pathname2url(filename)
            url = url.replace(r"/ext3-partition/ubuntu-home", r"/home")
            data = dict(mime_type=mime_type, app_name=app_name,
                        app_exec=app_exec)
            if not get_manager().add_full(url, data):
                raise RuntimeError("Error adding file")
        else:
            raise RuntimeError("Error: file does not exist.")

    def get_recent_files():
        items = get_manager().get_items()
        def cmp_items(i1, i2):
            return cmp(max(i2.get_visited(), i2.get_modified()),
                       max(i1.get_visited(), i1.get_modified()))
        items.sort(cmp=cmp_items)
        def fixup(url):
            uri = urlparse.unquote(url)
            #uri = i.get_uri()
            if uri.startswith("file://"):
                uri = uri[len("file://"):]
            return uri
        return [fixup(i.get_uri()) for i in items]

else:
    raise RuntimeError("Platform %s is not yet supported." % platform)
