export function compareJSON(jsonRaw1) {
  return function (jsonRaw2) {
    try {
      const json1 = JSON.parse(jsonRaw1);
      const json2 = JSON.parse(jsonRaw2);
      return compare(sanitize(json1), sanitize(json2));
    } catch (e) {
      console.error(e);
      return false;
    }
  };
}

function sanitize(json) {
  if (json === null || json === undefined) {
    return json;
  }

  if (Array.isArray(json)) {
    return json
      .filter((item) => item !== null && item !== undefined)
      .map((item) => sanitize(item));
  }

  if (typeof json === "object") {
    return Object.keys(json).reduce((acc, key) => {
      const value = json[key];
      if (
        value !== null &&
        value !== undefined &&
        !(Array.isArray(value) && value.length === 0)
      ) {
        acc[key] = sanitize(value);
      }
      return acc;
    }, {});
  }

  return json;
}

function compare(json1, json2) {
  if (json1 === json2) {
    return true;
  }

  if (
    typeof json1 !== "object" ||
    typeof json2 !== "object" ||
    json1 === null ||
    json2 === null
  ) {
    return false;
  }

  const keys1 = Object.keys(json1);
  const keys2 = Object.keys(json2);

  if (keys1.length !== keys2.length) {
    return false;
  }

  for (const key of keys1) {
    if (!keys2.includes(key) || !compare(json1[key], json2[key])) {
      return false;
    }
  }

  return true;
}
