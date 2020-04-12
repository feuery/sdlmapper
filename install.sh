local_projects_dir="$HOME/quicklisp/local-projects/"

if [[ ! -d "$local_projects_dir" ]]; then
   echo "$local_projects_dir not found. Please install quicklisp or change local_projects_dir variable in this shell script to point to quicklisp's local-projects directory"
   exit
fi

pushd $local_projects_dir
# this library doesn't support :default dispatching
# TODO write own and better
# or make a PR there
git clone https://github.com/strawhatguy/multimethods.git
popd

