## Agent Skills (SkillsJars)

This project declares Agent Skills as build-only dependencies in the sbt plugin's `Skills` configuration. Before working, extract them with:

```bash
./sbt extractSkillsJars
```

The generated skills are written under `.kiro/skills/`. Read relevant `SKILL.md` files there and follow their guidance. The generated directory is ignored because it can be reproduced from the pinned dependency in `build.sbt`.
