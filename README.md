
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ritalic

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![cran
checks](https://badges.cranchecks.info/worst/ritalic.svg)](https://cran.r-project.org/web/checks/check_results_ritalic.html)
[![cran
version](https://www.r-pkg.org/badges/version/ritalic)](https://cran.r-project.org/package=ritalic)
<!-- badges: end -->

**ritalic** is an R package which gives you access to
[ITALIC](https://italic.units.it/) mediated data via its [RESTful
API](https://italic.units.it/index.php?procedure=api).

**ITALIC** the Information System on Italian Lichens, makes available
information and resources about the lichens known to occur in Italy and
bordering countries. It is maintained and updated by the Research Unit
of Professor Pier Luigi Nimis, at the University of Trieste (NE Italy),
Department of Life Sciences. Most of the data are derived from the
Checklist of the Lichens of Italy by Nimis (2016), but nomenclatural and
distributional data are being continuously updated.

## Installation

You can either install the stable relase of ritalic using the version
available on CRAN:

``` r
# install ritalic
install.packages("ritalic")
# load ritalic
library(ritalic)
```

Or you can install the development version of the package from
[GitHub](https://github.com/plant-data/ritalic):

``` r
# install devtools package
install.packages("devtools")
# load devtools
library(devtools)
# install ritalic
devtools::install_github("plant-data/ritalic")
# load ritalic
library(ritalic)

# Now you can use the functions available in ritalic
```

## Usage

Italic has a guide designed to help users: [function and
examples](https://italic.units.it/ritalic/)

## Use ITALIC from AI assistants (MCP)

ritalic includes an [MCP](https://modelcontextprotocol.io) server, so that AI
assistants (Claude, Positron Assistant, GitHub Copilot in VS Code, ...) can align names, retrieve the data of the taxa,
search taxa by distribution, ecology and traits, and create identification keys,
running the functions of ritalic on your computer.

Install the packages needed by the server:

``` r
install.packages(c("mcptools", "ellmer"))
```

Then add the server to the assistant. Claude Code:

``` bash
claude mcp add ritalic -- Rscript -e "ritalic::italic_mcp_server()"
```

Claude Desktop, in `claude_desktop_config.json`:

``` json
{
  "mcpServers": {
    "ritalic": {
      "command": "Rscript",
      "args": ["-e", "ritalic::italic_mcp_server()"]
    }
  }
}
```

Positron (Posit Assistant), in `~/.posit/assistant/settings.json` for all
projects or `.posit/assistant/settings.json` for a single project:

``` json
{
  "mcpServers": {
    "ritalic": {
      "type": "local",
      "command": ["Rscript", "-e", "ritalic::italic_mcp_server()"],
      "timeout": 60000
    }
  }
}
```

The `timeout` (in milliseconds) gives R the time to start; type `/mcp` in the
assistant to check that the server is connected.

VS Code (GitHub Copilot chat, agent mode), in `.vscode/mcp.json` for the
workspace, or in the user configuration opened with the command
**MCP: Open User Configuration**:

``` json
{
  "servers": {
    "ritalic": {
      "type": "stdio",
      "command": "Rscript",
      "args": ["-e", "ritalic::italic_mcp_server()"]
    }
  }
}
```

Start the server with the command **MCP: List Servers** (or the Start button
shown above the server in `mcp.json`), then enable its tools in the tools
picker of the chat.

Use the full path of `Rscript` if the assistant cannot find it
(`file.path(R.home("bin"), "Rscript")` in R). The same tools can be used
in an `ellmer` chat with `chat$register_tools(italic_mcp_tools())`.

## Citation

Coming soon
