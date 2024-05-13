import os, sys
from os.path import abspath, join, split
from subprocess import call


user_dir = r"C:\Users\E038613"
start_menu_dir = r"AppData\Roaming\Microsoft\Windows\Start Menu\Programs"
emacs_home = user_dir
emacs_bin = r"C:\Users\E038613\emacs-24.3\bin"


def start_emacs(args):
    os.environ['HOME'] = emacs_home
    os.environ['PATH'] = emacs_bin + ';' + os.environ['PATH']
    cmd = '"' + join(emacs_bin, "runemacs") + '" --iconic'
    call(cmd, shell=True, env=os.environ)


def edit_files(args):
    os.environ['HOME'] = emacs_home
    os.environ['PATH'] = emacs_bin + ';' + os.environ['PATH']
    for f in args.files:
        f = f.replace('\\', '\\\\')
        cmd = '"' + join(emacs_bin, "emacsclient") + r'" --eval "(find-file \"{}\")"'.format(f)
        print cmd
        call(cmd, shell=True, env=os.environ)


def install(args):
    import os, sys
    import pythoncom
    from os.path import join, split, abspath
    from win32com.shell import shell, shellcon

    assert(sys.platform == "win32")

    windows_exec = os.path.join(split(sys.executable)[0], "pythonw.exe")

    shortcut = pythoncom.CoCreateInstance (
      shell.CLSID_ShellLink,
      None,
      pythoncom.CLSCTX_INPROC_SERVER,
      shell.IID_IShellLink
    )
    shortcut.SetPath(windows_exec)
    shortcut.SetDescription("Start Emacs")
    shortcut.SetIconLocation(join(emacs_bin, "runemacs.exe"), 0)
    shortcut.SetArguments('"{}" start'.format(abspath(__file__)))

    start_menu = shell.SHGetFolderPath(0, shellcon.CSIDL_STARTMENU, 0, 0)
    # desktop_path = shell.SHGetFolderPath (0, shellcon.CSIDL_DESKTOP, 0, 0)
    persist_file = shortcut.QueryInterface (pythoncom.IID_IPersistFile)
    persist_file.Save (os.path.join(start_menu, "Programs", "Emacs.lnk"), 0)

    import _winreg
    from os.path import abspath, join, split

    windows_exec = join(split(sys.executable)[0], "pythonw.exe")
    cmd = '"{}" "{}" edit "%l"'.format(windows_exec, abspath(__file__))

    try:
        key = _winreg.OpenKey(_winreg.HKEY_CLASSES_ROOT, "*\Shell\Open in emacs")
    except WindowsError:
        key = _winreg.CreateKey(_winreg.HKEY_CLASSES_ROOT, "*\Shell\Open in emacs")

    _winreg.SetValue(key, "Command", _winreg.REG_SZ, cmd)

    try:
        key = _winreg.OpenKey(_winreg.HKEY_CLASSES_ROOT, "Directory\Shell\Open in emacs")
    except WindowsError:
        key = _winreg.CreateKey(_winreg.HKEY_CLASSES_ROOT, "Directory\Shell\Open in emacs")

    _winreg.SetValue(key, "Command", _winreg.REG_SZ, cmd)


if __name__ == '__main__':
    from argparse import ArgumentParser
    parser = ArgumentParser("This is a script for opening files in emacs.")

    subparsers = parser.add_subparsers(help='possible sub-commands')

    # 'edit' command
    subparser = subparsers.add_parser('edit', help='Edit the file in emacs.')
    subparser.add_argument('files', nargs='*', type=str, help='filenames')
    subparser.set_defaults(func=edit_files)

    # 'install' command
    subparser = subparsers.add_parser('install', help='Install windows stuff')
    subparser.set_defaults(func=install)

    # 'start' command
    subparser = subparsers.add_parser('start', help='Install windows stuff')
    subparser.set_defaults(func=start_emacs)

    # # 'add-right-click-menu'
    # help_ = 'Create an "Open with emacs" right-click menu in explorer.'
    # subparser = subparsers.add_parser('add-right-click-menu', help=help_)
    # subparser.set_defaults(func=add_right_click_menu)
    # # Execute the command

    print
    args = parser.parse_args()
    args.func(args)
