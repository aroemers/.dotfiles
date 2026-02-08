#!/usr/bin/env bash

# gwt - Git WorkTree management function
# Usage: gwt <branch-name>

gwt() {
    local branch_name="$1"
    local worktree_path=".worktrees/$branch_name"

    # Validate input
    if [[ -z "${branch_name:-}" ]]; then
        echo "gwt: Missing branch name" >&2
        echo "Usage: gwt <branch-name>" >&2
        return 1
    fi

    # Get main repository root (works from any worktree)
    local main_repo_root=$(dirname "$(git rev-parse --git-common-dir)") || return 1

    # Create the worktree - let git handle everything
    if (cd "$main_repo_root" && git worktree add --relative-paths -B "$branch_name" "$worktree_path"); then
        echo "✓ Worktree created successfully"
        echo "  Branch: $branch_name"
        echo "  Path: $worktree_path"
        echo "  Navigated to: $main_repo_root/$worktree_path"

        # Navigate to the new worktree
        cd "$main_repo_root/$worktree_path"
    else
        echo "gwt: Failed to create worktree" >&2
        return 1
    fi
}
