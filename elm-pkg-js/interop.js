const version = "v1";
const TO_JS_PORT = "toJs";
const TO_ELM_PORT = "toElm";
const SESSION_POOL_NAME_KEY = "sessionPoolName";

exports.init = async function (app) {
  // Initial load of data
  try {

    const sessionPoolNameStored = localStorage.getItem(SESSION_POOL_NAME_KEY);
    const sessionPoolName = sessionPoolNameStored ? JSON.parse(sessionPoolNameStored) : null;

    // Create the message object
    const message = {
      tag: "InitData",
      data: {
        poolName: sessionPoolName,
        version: version,
      },
    };

    console.log("Sending to Elm:", message);

    // Convert to string before sending through port
    app.ports[TO_ELM_PORT].send(JSON.stringify(message));
  } catch (error) {
    console.error("Error loading initial data:", error);
  }

  app.ports[TO_JS_PORT].subscribe(async function (event) {
    console.log("fromElm", JSON.stringify(event));

    if (event.tag === undefined || event.tag === null) {
      console.error("fromElm event is missing a tag", event);
      return;
    }

    switch (event.tag) {
      case "StoreSessionPoolName":
        localStorage.setItem(SESSION_POOL_NAME_KEY, JSON.stringify(event.data));
        break;
      default:
        console.log(`fromElm event of tag ${event.tag} not handled`, event);
    }
  });
  setupServiceworker();
};

function setupServiceworker() {
  if ("serviceWorker" in navigator) {
    window.addEventListener("load", async () => {
      try {
        const registration = await navigator.serviceWorker.register(
          "/serviceWorker.js"
        );
        console.log("ServiceWorker registration successful");
      } catch (err) {
        console.log("ServiceWorker registration failed: ", err);
      }
    });
  }
}