# org-linker.el

Attach a file as an [org-mode
link](https://orgmode.org/manual/External-Links.html) associated
with a dynamically generated UUID. Ideal for enhancing
note-taking in org-mode with reliable linkages.

## Usage

### Attach and Link

Use `M-x org-linker/attach-file-with-uuid` to attach a file or a
folder at point as an org-mode link identified by a UUID
generated on-the-fly. The file or folder is then copied to the
directory named after that UUID under `org-linker`'s root
directory. Press `<RET>` on the org link to open the file. You
can customize the uuid generating function (see below).
   
### Delete and Remove

Execute `M-x org-linker/trash-folder-uuid` to select the UUID and
the corresponding folder for removal. Alternatively, you can
remove a UUID and its folder by running `M-x
org-linker/trash-folder-at-point` at the org link. You can
customize the trashing function (see below).
   
### Transaction History

All transactions are logged in a human-readable text-based
database under `org-linker`'s root directory.

### Example

For instance, if the root is `/tmp/org-linker/`, and the UUID is
`20240101-235959`, and the file is `readme.md`, the file will be
copied to `/tmp/org-linker/20240101-235959/readme.md` with the
transaction recorded in `/tmp/org-linker/db.tx`. And an org-link
`[[linker:20240101-235959/readme.md]]` is inserted at point.

## Comparison with org-attach

`org-linker` differs from `org-attach` in its approach to file
attachment. While `org-attach` uses an org heading as a basic
storing unit, which can lead to issues if not managed carefully,
`org-linker` assigns a unique UUID to each attached file,
ensuring a more robust linkage system. By using individual files
as the basic unit, `org-linker` offers a safer and more reliable
way to manage attachments within org-mode documents.

Both tools have their strengths and are suitable for different
use cases. However, if you prioritize a secure and
straightforward attachment system, org-linker might be the
preferred choice.

## Customizable Variables

+ `org-linker/root-directory`

  Default: `"/tmp/org-linker/"`
  
  The root directory where attachments are stored in org-linker.
  
+ `org-linker/transaction-file-name`
  
  Default: `"db.tx"`
  
  The file name that holds the transaction history.

+ `org-linker/file-size-limit`

  Default: 50000
  
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

### TODOs

+ [ ] Publish the package to MELPA or another reputable package repository.
+ [ ] Support inline image display.
