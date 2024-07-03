#!/bin/sh

pre_push_hook=".git/hooks/pre-push"
pre_push_sample=".git/hooks/pre-push.sample"
pre_commit_hook=".git/hooks/pre-commit"
pre_commit_sample=".git/hooks/pre-commit.sample"

# Function to ensure hook file exists by moving the sample or creating an empty file
ensure_hook_file_exists() {
  hook_file=$1
  sample_file=$2

  if [ ! -f "$hook_file" ]; then
    if [ -f "$sample_file" ]; then
      mv "$sample_file" "$hook_file"
    else
      touch "$hook_file"
    fi
  fi
}

# Function to add command to the beginning of a hook file
add_command_to_hook() {
  hook_file=$1
  command=$2

  new_command="#!/bin/sh
$command
"
  {
    echo "$new_command"
    cat "$hook_file"
  } > "${hook_file}.tmp"

  mv "${hook_file}.tmp" "$hook_file"
  chmod +x "$hook_file"
}

ensure_hook_file_exists "$pre_push_hook" "$pre_push_sample"
ensure_hook_file_exists "$pre_commit_hook" "$pre_commit_sample"

add_command_to_hook "$pre_push_hook" "cabal test"

pre_commit_commands=$(cat <<'EOF'
ormolu --mode inplace $(find . -name '*.hs')
git add .
echo "format the code and then run cmamnd 'git add .'"
EOF
)

add_command_to_hook "$pre_commit_hook" "$pre_commit_commands"