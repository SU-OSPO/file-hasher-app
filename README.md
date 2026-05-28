# File hash renamer

Shinylive app that takes a batch of files, hashes each with SHA-256, and returns:

- a ZIP of the files renamed to `<id>.<original-extension>`
- `filename-map.csv` mapping new name → old name → full SHA-256
- `ids.csv` with the bare IDs, one per row

Runs entirely in the browser via WebAssembly — uploads never leave the client.

## Develop

```r
shiny::runApp("app")
```

Requires `shiny`, `digest`, and `zip`.

## Deploy

Push to `main` → `.github/workflows/deploy.yaml` runs `shinylive::export("app", "site")` and publishes to GitHub Pages. Pull requests build but don't deploy.

## Support

Developed and maintained by the [Open Source Program Office](https://opensource.syracuse.edu/) at Syracuse University. Reach out for feedback and suggested improvements:

- GitHub Issues: https://github.com/SU-OSPO/file-hasher-app/issues
- Email: ospo@syr.edu

## Acknowledgments

This project was supported as part of a grant (#[G2023-20946](https://sloan.org/grant-detail/G-2023-20946)) from the Alfred P. Sloan Foundation.
