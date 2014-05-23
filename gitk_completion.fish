# fish completion for gitk

function __fish_gitk_branches
  command git branch --no-color -a ^/dev/null | sgrep -v ' -> ' | sed -e 's/^..//' -e 's/^remotes\///'
end

function __fish_gitk_tags
  command git tag
end

function __fish_gitk_heads
  __fish_gitk_branches
  __fish_gitk_tags
end

complete -c gitk -a '(__fish_gitk_heads)' -f -d "Head"
complete -c gitk -a '(git ls-tree HEAD --name-only)' -d "_"
