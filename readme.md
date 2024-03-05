<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [🔗 org-linker.el 🔗 ](#🔗-org-linkerel-🔗)
    - [Comparison with org-attach](#comparison-with-org-attach)
    - [Usage](#usage)
        - [Attach and Link](#attach-and-link)
        - [Transaction History](#transaction-history)
        - [Example](#example)
        - [Delete and Remove](#delete-and-remove)
        - [Inline Images Support](#inline-images-support)
    - [Customizable Variables](#customizable-variables)
    - [Contributions](#contributions)

<!-- markdown-toc end -->

# 🔗 org-linker.el 🔗 

Attach a file as an [org-mode
link](https://orgmode.org/manual/External-Links.html) associated
with a dynamically generated UUID. Ideal for enhancing
note-taking in org-mode with reliable linkages.

## Comparison with org-attach

`org-linker` differs from `org-attach` in its approach to file
attachment. 

`org-attach` uses an org heading as a basic storing unit, which
can lead to issues if not managed carefully (e.g. refiling or
adding shadowing subtrees). 

On the other hand, `org-linker` assigns a unique UUID to each
attached file, ensuring a more robust linkage system. By treating
individual files as the fundamental unit, `org-linker` provides a
**safer and more flexible** approach to handling attachments in
org-mode documents. For example, this allows **easy movement
and copying of links across various org files and headings.**

Both tools have their strengths and are suitable for different
use cases. However, if you prioritize a secure and
straightforward attachment system, org-linker might be the
preferred choice.

## Usage

#### Attach and Link

Use `M-x org-linker/attach-file-with-uuid` to attach a file or a
folder as an org-mode link identified by a UUID (for this
transaction) generated on-the-fly. The file or folder is then
copied to the directory named after that UUID under
`org-linker`'s root directory. Press `<RET>` on the org link to
open the file. You can customize the uuid generating function
`org-linker/uuid-generator-function` (see below).
   
#### Transaction History

All transactions are logged in a human-readable text-based
database under `org-linker`'s root directory. So if you do
something wrong, you can always go to the root and resolve any
problem with ease.

#### Example

For instance, if the root is `/tmp/org-linker/`, and the UUID of
the transaction is `20240101-235959`, and the file is
`readme.md`, the file will be copied to
`/tmp/org-linker/20240101-235959/readme.md` with the transaction
recorded in `/tmp/org-linker/db.tx`. And an org-link
`[[linker:20240101-235959/readme.md]]` is inserted at point.

#### Delete and Remove

Execute `M-x org-linker/trash-folder-uuid` to select the UUID and
the corresponding folder for removal. Alternatively, you can
remove a UUID and its folder by running `M-x
org-linker/trash-folder-at-point` at the org link. You can
customize the trashing function `org-linker/trashing-function`
(see below).

#### Inline Images Support 

Support org inline images. Enabling this feature redefines
`#'org-display-inline-images` and `#'+org/dwim-at-point'`, so for
now you need to enable it by evaluating
`(org-linker/support-org-inline-image)`.
   
## Customizable Variables

+ `org-linker/root-directory`

  Default: `"/tmp/org-linker/"`
  
  The root directory where attachments are stored in org-linker.
  It is expected to be an absolute path. Tips: You can use
  `(concat (getenv "HOME") "/path/to/root/")`.
  
+ `org-linker/transaction-file-name`
  
  Default: `"db.tx"`
  
  The file name that holds the transaction history.

+ `org-linker/file-size-limit`

  Default: `50000`
  
  The maximum file or directory size to be copied (in kilobytes).

+ `org-linker/uuid-generator-function`
  
  Default: `(lambda () (shell-command-to-string "uuidgen | cut -d'-' -f1-2 | tr -d '\n'"))`
  
  This variable stores the function responsible for generating a
  UUID. Users are encouraged to customize this function according
  to their needs. In practice, this function is invoked by
  #'org-linker/generate-unique-uuid to ensure the generation of
  unique UUIDs without conflicts.

+ `org-linker/trashing-function`
  
  Default: `(lambda (file) (call-process "trash-put" nil 0 nil file))`

    (The lambda function moves FILE to the system trash using the
    `trash-put` command line app.)
             
  This variable stores the function responsible for trashing a
  path. It is expected to take one parameter.
 
## Contributions

Feel free to contribute by submitting pull requests, raising
issues, or sharing your ideas.

Help is needed! The higher the more urgent.

+ [ ] Publish the package to [MELPA](https://github.com/melpa/melpa/blob/master/CONTRIBUTING.org) 
      or another reputable package repository.
+ [ ] Support symlink method.
+ [ ] Support attachment to a pre-selected UUID (fuzzy search through existing UUIDs).
+ [ ] Support users to fuzzy search through the UUIDs based on the name of an attached file or folder.
+ [X] Support inline image display.
+ [ ] Create a logo.
