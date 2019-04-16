# n2wal - Miniflux -> Wallabag syncer for emacs

This is a small emacs package that implements a two-way sync between
miniflux and wallabag.

## Setup

1. Create initial configuration:

   From a shell:

       $ emacs -q -l n2wal.el -f n2wal-create-config

   Or from within emacs: `M-x n2wal-create-config`

2. Select miniflux feeds to be synced:

   From within a shell:

       $ ./n2wal-add-feed

   Or from within emacs: `M-x n2wal-add-feed`. If helm is available,
   that will be used in stead of ido for selection.

## Running the sync job

From a shell:

     $ ./n2wal

From within emacs: `M-x n2wal-sync-feeds`
