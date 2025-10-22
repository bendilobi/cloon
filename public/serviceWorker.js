// Increment version when you update any of the local resources, which will
// in turn trigger the install event again.
const PRECACHE = "precache-v0.8.16";

// A list of local resources we always want to be cached.
const PRECACHE_URLS = [ 
    // "/",
    "/registerServiceWorker.js",
    // "/app.css",
    "/icons/favicon-512x512.png",
    "/icons/favicon-16x16.png",
    "/icons/favicon-32x32.png",
    "/icons/favicon-192x192.png",
    "/manifest.json",
    "/favicon.ico",
  ];

// The install handler takes care of precaching the resources we always need.
self.addEventListener("install", (event) => {
  event.waitUntil(
    caches
      .open(PRECACHE)
      .then((cache) => cache.addAll(PRECACHE_URLS))
      .then(self.skipWaiting())
  );
});

// The activate handler takes care of cleaning up old caches.
self.addEventListener("activate", (event) => {
  const currentCaches = [PRECACHE, ELMCACHE];
  event.waitUntil(
    caches
      .keys()
      .then((cacheNames) => {
        return cacheNames.filter(
          (cacheName) => !currentCaches.includes(cacheName)
        );
      })
      .then((cachesToDelete) => {
        return Promise.all(
          cachesToDelete.map((cacheToDelete) => {
            return caches.delete(cacheToDelete);
          })
        );
      })
      .then(() => self.clients.claim())
  );
});

self.addEventListener("fetch", (event) => {
  console.log(`URL requested: ${event.request.url}`);

  if (event.request.url.startsWith(self.location.origin) 
    && !event.request.url.includes("serviceWorker.js")) {

    event.respondWith(
      caches.match(event.request).then((cachedResponse) => {
          return fetch(event.request).then((response) => {
            if (response.ok) {
              // Only put valid stuff in caches

              const responseClone = response.clone();

              caches.open(PRECACHE).then((cache) => {
                cache.put(event.request, responseClone);
              });
              return response;
            } else if (cachedResponse !== undefined) {
              return cachedResponse;
            }
            
          });
      })
    )
  }
});