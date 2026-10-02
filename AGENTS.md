## Agent Skills (SkillsJars)

This project declares Agent Skills as build-only dependencies in the sbt plugin's `Skills` configuration. Before working, extract them with:

```bash
./sbt extractSkillsJars
```

The generated skills are written under `.kiro/skills/`. Read relevant `SKILL.md` files there and follow their guidance. The generated directory is ignored because it can be reproduced from the pinned dependency in `build.sbt`.

## Agent tooling

- Follow the `zen-of-projects` Skill (extract with `./sbt extractSkillsJars` into the gitignored `.kiro/skills/`); this file records only project-specific facts and exceptions.
- MCP server `sbt-mcp-jev-llm-connect-four` (sbt-mcp) listens on `http://127.0.0.1:5014/`. Kiro uses the HTTP entry in `.kiro/settings/mcp.json`; start sbt first. Claude Code uses `.mcp.json`, which runs `.claude/sbt-mcp-stdio.sh` (approved in `.claude/settings.json`). That stdio bridge starts a foreground sbt in cloud sessions (`CLAUDE_CODE_REMOTE=true`), and locally only connects to an sbt you already started. Its tools are deferred: load them with ToolSearch (search `sbt-mcp-jev-llm-connect-four`). Diagnostics go to `/tmp/sbt-mcp-stdio.log` and `/tmp/sbt-mcp-server.log`.
- Maintenance routine: `.factory/MAINTENANCE.md` (weekly), following the `zen-of-projects` Skill.
