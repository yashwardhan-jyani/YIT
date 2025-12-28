import os
import grp, pwd
from datetime import datetime

import utils as git_utils


def init(args):
    git_utils.repo_create(args.path)


def hash_object(args):
    if args.write:
        repo = git_utils.repo_find()
    else:
        repo = None

    with open(args.path, 'rb') as fd:
        sha = git_utils.object_hash(fd, args.type.encode(), repo)
        print(sha)


def cat_file(args):
    repo = git_utils.repo_find()
    git_utils.cat_file_helper(repo, args.object, fmt=args.type.encode())


def log(args):
    repo = git_utils.repo_find()

    print("digraph log{")
    print("  node[shape=rect]")
    git_utils.log_graphviz(repo, git_utils.object_find(repo, args.commit), set())
    print("}")


def ls_tree(args):
    repo = git_utils.repo_find()
    git_utils.ls_tree(repo, args.tree, args.recursive)


def checkout(args):
    repo = git_utils.repo_find()

    obj = git_utils.object_read(repo, git_utils.object_find(repo, args.commit))

    # If the object is a commit, we grab its tree
    if obj.fmt == b'commit':
        obj = git_utils.object_read(repo, obj.kvlm[b'tree'].decode("ascii"))

    # Verify that path is an empty directory
    if os.path.exists(args.path):
        if not os.path.isdir(args.path):
            raise Exception(f"Not a directory {args.path}!")
        if os.listdir(args.path):
            raise Exception(f"Not empty {args.path}!")
    else:
        os.makedirs(args.path)

    git_utils.tree_checkout(repo, obj, os.path.realpath(args.path))

def show_ref(args):
    repo = git_utils.repo_find()
    refs = git_utils.ref_list(repo)
    git_utils.show_ref(repo, refs, prefix='refs')

def tag(args):
    repo = git_utils.repo_find()

    if args.name:
        git_utils.tag_create(repo, args.name, args.object, args.create_tag_object)
    else:
        refs = git_utils.ref_list(repo)
        git_utils.show_ref(repo, refs["tags"], with_hash=False)

def rev_parse(args):
    print(args)
    if args.type:
        fmt = args.type.encode()
    else:
        fmt = None

    repo = git_utils.repo_find()

    print (git_utils.object_find(repo, args.name, fmt, follow=True))

def ls_files(args):
    repo = git_utils.repo_find()
    index = git_utils.index_read(repo)
    if args.verbose:
        print(f"Index file format v{index.version}, containing {len(index.entries)} entries.")

    for e in index.entries:
        print(e.name)
        if args.verbose:
            entry_type = {
                0b1000: "regular file",
                0b1010: "symlink",
                0b1110: "gitlink"
            }[e.mode_type]
            print(f"  {entry_type} with perms: {e.mode_perms:o}")
            print(f"  on blob: {e.sha}")
            print(f"  created: {datetime.fromtimestamp(e.ctime[0])}.{e.ctime[1]}, modified: {datetime.fromtimestamp(e.mtime[0])}.{e.mtime[1]}")
            print(f"  device: {e.dev}, inode: {e.ino}")
            print(f"  user: {pwd.getpwuid(e.uid).pw_name} ({e.uid})  group: {grp.getgrgid(e.gid).gr_name} ({e.gid})")
            print(f"  flags: stage={e.flag_stage} assume_valid={e.flag_assume_valid}")

def check_ignore(args):
    repo = git_utils.repo_find()
    rules = git_utils.gitignore_read(repo)
    for path in args.path:
        if git_utils.check_ignore(rules, path):
            print(path)

def status(_):
    repo = git_utils.repo_find()
    index = git_utils.index_read(repo)

    git_utils.status_branch(repo)
    git_utils.status_head_index(repo, index)
    print()
    git_utils.status_index_worktree(repo, index)

def rm(args):
    repo = git_utils.repo_find()
    git_utils.rm(repo, args.path)

def add(args):
    repo = git_utils.repo_find()
    git_utils.add(repo, args.path)

def commit(args):
    repo = git_utils.repo_find()
    index = git_utils.index_read(repo)
    # Create trees, grab back SHA for the root tree.
    tree = git_utils.tree_from_index(repo, index)

    # Create the commit object itself
    commit = git_utils.commit_create(
        repo, tree, git_utils.object_find(repo, "HEAD"),
        git_utils.gitconfig_user_get(git_utils.gitconfig_read()),
        datetime.now(), args.message
    )

    # Update HEAD so our commit is now the tip of the active branch.
    active_branch = git_utils.branch_get_active(repo)
    if active_branch: # If we're on a branch, we update refs/heads/BRANCH
        with open(git_utils.repo_file(repo, os.path.join("refs/heads", active_branch)), "w") as fd:
            fd.write(commit + "\n")
    else: # Otherwise, we update HEAD itself.
        with open(git_utils.repo_file(repo, "HEAD"), "w") as fd:
            fd.write("\n")
