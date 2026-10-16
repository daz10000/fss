# Releasing (maintainers)

Releases are published to nuget.org by [`.github/workflows/release.yml`](../.github/workflows/release.yml)
when a `v*` tag is pushed. It authenticates with **NuGet Trusted Publishing**: GitHub Actions' OIDC token
is exchanged for a NuGet API key that is valid for one hour, so no long-lived API key is stored anywhere.

Published packages: `fss_duckdb`, `fss_postgres`, `fss_sqlite`, `fss`. They share one version, set by
`<VersionPrefix>` in `Directory.Build.props`. `fss_mysql` is retired (`IsPackable=false`).

## One-time setup

You must do these steps yourself on nuget.org and GitHub; nothing in the repo can do them.

1. **nuget.org trusted publishing policy.** Sign in as the account that owns the packages. Open the
   account menu, then **Trusted Publishing**, then **Create**, and fill in:
   - Package owner: your account (or the organization that owns the packages)
   - Repository owner: `daz10000`
   - Repository: `fss`
   - Workflow file: `release.yml` (the file name only, not the path)
   - Environment: `nuget`

   The policy applies to every package owned by that package owner.
2. **GitHub environment.** In repo **Settings**, then **Environments**, create `nuget`. Recommended:
   restrict deployments to tags matching `v*`, and add yourself as a required reviewer. Every publish
   then waits for one click of approval.
3. **Secret.** In the `nuget` environment (or the repo), add the secret **`NUGET_USER`**. Its value is
   your nuget.org *username* (profile name), not your email address.
4. **Retire old keys.** If a long-lived nuget.org API key was ever used for these packages (for
   example by the old FAKE/Paket `PublishNuget` target), revoke it at nuget.org, under
   **API Keys**.
5. Optional, on nuget.org: mark `fss_mysql` **deprecated** ("Legacy", with alternatives `fss_duckdb`,
   `fss_postgres`). Consider a note in `fss`'s description that it's a legacy package (already in its
   readme).

## Each release

1. Make sure `master` is green in CI.
2. Set `<VersionPrefix>` in `Directory.Build.props` (e.g. `2.1.0`, or `2.2.0-beta1` for a prerelease).
3. In `RELEASE_NOTES.md`, change the top heading to `## 2.1.0 - YYYY-MM-DD`. The workflow copies this
   section into the GitHub release.
4. Commit and merge to `master`.
5. Tag and push:

   ```sh
   git tag v2.1.0
   git push origin v2.1.0
   ```

6. The workflow checks that the tag matches `VersionPrefix`, builds, tests (DuckDB and SQLite), packs,
   logs in to NuGet with OIDC, pushes `.nupkg` and `.snupkg`, and creates a GitHub release with the
   packages attached. A version that is already published is skipped (`--skip-duplicate`), so a
   failed run can be re-run safely.

## Checking a build before release

Every CI run uploads the packages, versioned `X.Y.Z-ci.N`, as the `packages` artifact. To try them in
another project:

```sh
dotnet nuget add source /path/to/downloaded/packages --name fss-local
dotnet add package fss_duckdb --version 2.1.0-ci.42
```

## If publishing fails

- **`NuGet/login` fails with 401/403.** The policy doesn't match the run. Check the repository owner
  and name, that the workflow file name is exactly `release.yml`, that the environment name is exactly
  `nuget`, and that `NUGET_USER` is the username, not the email address. A newly created policy may
  show as pending on nuget.org until its first successful use.
- **The push fails with 403 for one package.** The policy's package owner doesn't own that package ID.
  Add the account as an owner of the package on nuget.org.
- **The tag doesn't match the version.** Fix `VersionPrefix`, delete the tag
  (`git push --delete origin vX.Y.Z` and `git tag -d vX.Y.Z`), and tag again.
