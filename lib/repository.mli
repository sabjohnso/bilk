(** Remote repository management.

    Manages remote git-hosted package repositories.  Repositories are
    configured in [~/.wile/repositories] and cached as shallow clones
    under [~/.wile/repos/].

    {b Repository layout:}
    {v
    index.scm              (optional)
    packages/
      <name>/
        <version>/
          package.scm
          src/
            <.sld files>
    v}

    {b Config format} ([~/.wile/repositories]):
    {v
    ((default . "https://github.com/wile-scheme/packages"))
    v}
    Alist mapping repo name (symbol) to URL (string). *)

(** {1 Exceptions} *)

exception Repository_error of string

(** {1 Types} *)

type repo = {
  name : string;
  url : string;
}

type index_entry = {
  pkg_name : string;
  versions : Semver.t list;
}

type index = {
  repo_name : string;
  entries : index_entry list;
}

(** {1 Configuration} *)

val repos_dir : string -> string
(** [repos_dir wile_home] returns the directory for cached clones:
    [wile_home/repos/]. *)

val repos_config_path : string -> string
(** [repos_config_path wile_home] returns the path to the repositories
    config file: [wile_home/repositories]. *)

val load_repos : string -> repo list
(** [load_repos wile_home] reads the repository configuration.
    Returns [\[\]] if the config file is missing.
    @raise Repository_error if the file is malformed. *)

val save_repos : string -> repo list -> unit
(** [save_repos wile_home repos] writes the repository configuration. *)

(** {1 Clone management} *)

val clone_dir : string -> repo -> string
(** [clone_dir wile_home repo] returns the local clone path:
    [wile_home/repos/<name>/]. *)

val sync : string -> repo -> unit
(** [sync wile_home repo] clones or pulls the repository.
    @raise Repository_error on git failure. *)

(** {1 Index} *)

val parse_index : Readtable.t -> string -> index
(** [parse_index readtable content] parses an index from a string.
    @raise Repository_error if the index is malformed. *)

val load_index : string -> repo -> index option
(** [load_index wile_home repo] reads [index.scm] from the clone.
    Returns [None] if the file does not exist. *)

(** {1 Package access} *)

val package_dir :
  string -> repo -> name:string -> version:string -> string
(** [package_dir wile_home repo ~name ~version] returns the path to
    a package version within the cloned repo. *)

val has_package :
  string -> repo -> name:string -> version:string -> bool
(** [has_package wile_home repo ~name ~version] checks whether the
    package version directory exists in the clone. *)

val scan_versions : string -> repo -> string -> Semver.t list
(** [scan_versions wile_home repo name] lists available versions
    of a package in the cloned repo, sorted ascending. *)

val fetch_package :
  wile_home:string -> registry_root:string -> repo ->
  name:string -> version:string -> unit
(** [fetch_package ~wile_home ~registry_root repo ~name ~version]
    installs a package from the cloned repo into the local registry.
    @raise Repository_error if the package is not in the repo. *)

(** {1 Search} *)

val search_index : index -> string -> index_entry list
(** [search_index idx query] returns entries whose name contains
    [query] (case-insensitive substring match). *)

val search_all : string -> repo list -> string -> (repo * index_entry) list
(** [search_all wile_home repos query] searches all repos for matching
    packages.  Skips repos without an index. *)
