import argparse
import sys

import main as main_funcs

argparser = argparse.ArgumentParser(description='Reinvent Git')

argsubparsers = argparser.add_subparsers(title='Commands', dest='command')
argsubparsers.required = True


argsp = argsubparsers.add_parser('init', help='Initialize a new, empty repository.')
argsp.add_argument(
    'path', metavar='directory', nargs='?', default='.', help='Where to create the repository.'
)

argsp = argsubparsers.add_parser('cat-file', help='Procide content of the repository objects.')
argsp.add_argument(
    'type', choices=['blob', 'commit', 'tag', 'tree'], help='Specify the type'
)
argsp.add_argument(
    'object', metavar='object', help='The object to display'
)

argsp = argsubparsers.add_parser('hash-object', help='Compute object ID and optionally creates a blob from a file')
argsp.add_argument(
    '-t', metavar='type', dest='type', choices=['blob', 'commit', 'tag', 'tree'],
    default='blob', help='Specify the type'
)
argsp.add_argument(
    '-w', dest='write', action='store_true', help='Actually write the object into the database'
)
argsp.add_argument(
    'path', help='Read object from <file>'
)

argsp = argsubparsers.add_parser("log", help="Display history of a given commit.")
argsp.add_argument(
    "commit", default="HEAD", nargs="?", help="Commit to start at."
)

argsp = argsubparsers.add_parser("ls-tree", help="Pretty-print a tree object.")
argsp.add_argument(
    "-r", dest="recursive", action="store_true", help="Recurse into sub-trees"
)
argsp.add_argument(
    "tree", help="A tree-ish object."
)

argsp = argsubparsers.add_parser("checkout", help="Checkout a commit inside of a directory.")
argsp.add_argument(
    "commit", help="The commit or tree to checkout."
)
argsp.add_argument(
    "path", help="The EMPTY directory to checkout on."
)

argsp = argsubparsers.add_parser("show-ref", help="List references in a local repository.")

argsp = argsubparsers.add_parser("tag", help="Create, list a tag object signed with GPG.")
argsp.add_argument(
    "-a", action="store_true", dest="create_tag_object", help="Whether to create a tag object"
)
argsp.add_argument(
    "name", nargs="?", help="The new tag's name"
)
argsp.add_argument(
    "object", default="HEAD", nargs="?", help="The object the new tag will point to"
)

argsp = argsubparsers.add_parser(
    "rev-parse", help="Parse revision (or other objects) identifiers"
)
argsp.add_argument(
    "--yit-type",metavar="type", dest="type",choices=["blob", "commit", "tag", "tree"],
    default=None,help="Specify the expected type"
)
argsp.add_argument(
    "name", help="The name to parse"
)

argsp = argsubparsers.add_parser("ls-files", help = "List all the stage files")
argsp.add_argument("--verbose", action="store_true", help="Show everything.")

argsp = argsubparsers.add_parser("check-ignore", help = "Check path(s) against ignore rules.")
argsp.add_argument("path", nargs="+", help="Paths to check")

argsp = argsubparsers.add_parser("status", help = "Show the working tree status.")

argsp = argsubparsers.add_parser("rm", help="Remove files from the working tree and the index.")
argsp.add_argument("path", nargs="+", help="Files to remove")

argsp = argsubparsers.add_parser("add", help = "Add files contents to the index.")
argsp.add_argument("path", nargs="+", help="Files to add")

argsp = argsubparsers.add_parser("commit", help="Record changes to the repository.")
argsp.add_argument(
    "-m", metavar="message", dest="message",
    help="Message to associate with this commit."
)


def main(argv=sys.argv[1:]):
    args = argparser.parse_args(argv)
    match args.command:
        case 'add'          : main_funcs.add(args)
        case "cat-file"     : main_funcs.cat_file(args)
        case "check-ignore" : main_funcs.check_ignore(args)
        case "checkout"     : main_funcs.checkout(args)
        case "commit"       : main_funcs.commit(args)
        case "hash-object"  : main_funcs.hash_object(args)
        case "init"         : main_funcs.init(args)
        case "log"          : main_funcs.log(args)
        case "ls-files"     : main_funcs.ls_files(args)
        case "ls-tree"      : main_funcs.ls_tree(args)
        case "rev-parse"    : main_funcs.rev_parse(args)
        case "rm"           : main_funcs.rm(args)
        case "show-ref"     : main_funcs.show_ref(args)
        case "status"       : main_funcs.status(args)
        case "tag"          : main_funcs.tag(args)
        case _              : print("Bad command.")
