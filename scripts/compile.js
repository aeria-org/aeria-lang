const util = require("node:util");
const { exec } = require("node:child_process");

const execute = util.promisify(exec);

async function main() {
  try {
    const args = process.argv.slice(2);
    if (args.length !== 3) {
      console.error(
        "Usage: node compile.js <filepath> <outputpath> <commonjs|esnext>"
      );
      process.exit(1);
    }
    const [filepath, outputpath, module_] = args;

    console.log("[info] Running spago...");
    await execute(`npx spago run -b "${filepath} ${outputpath} ${module_}"`);

    console.log("[info] Running package.js...");
    await execute(`node ./scripts/package.js ${outputpath}`);
  } catch (error) {
    console.error("[error] ", error);
    process.exit(1);
  }
}

main();
