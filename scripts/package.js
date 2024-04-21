const fs = require("node:fs/promises");
const path = require("node:path");

async function generate_package(filepath) {
  const package = {
    exports: {},
  };

  const files = await fs.readdir(filepath);
  for (const file of files) {
    const fullPath = path.join(filepath, file);
    const stats = await fs.stat(fullPath);

    if (!stats.isDirectory()) {
      const extension = path.extname(file);
      const baseName = path.basename(file, extension);
      if (baseName === "package") continue;

      const declarationType = baseName.includes(".d") ? "types" : null;
      const key = `./collections/${baseName.replace(".d", "")}`;

      package.exports[key] = package.exports[key] || {};
      package.exports[key] = {
        ...package.exports[key],
        require: `./${baseName}.js`,
        import: `./${baseName}.mjs`,
      };
      if (declarationType) {
        package.exports[key][declarationType] = `./${baseName}.ts`;
      }
    }
  }

  return package;
}

async function main() {
  try {
    const args = process.argv.slice(2);
    if (args.length !== 1) {
      console.error("Usage: node package.js <filepath>");
      process.exit(1);
    }
    const [filepath] = args;

    const package = await generate_package(filepath);
    await fs.writeFile(
      path.join(filepath, "package.json"),
      JSON.stringify(package, null, 2)
    );
    console.log("[info] package.json generated successfully");
  } catch (error) {
    console.error("[error] ", error);
    process.exit(1);
  }
}

main();
