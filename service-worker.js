//
// I have no particular need for a service worker, but it's necessary for a PWA to work at all.
// There are a few referenced images, but the app is fine without them.
// There's nothing to pre-fetch because the page references all of its resources when
// loaded. But we can still have some fun optimizing the upgrade process.
//
// This is also an experiment to see whether async functions simplify writing service workers.
// Guess what? They do!
//
// Copyright 2021 Stan Switzer
//   This work is licensed under a Creative Commons Attribution-ShareAlike
//   4.0 International License. https://creativecommons.org/licenses/by-sa/4.0/
//

let cacheName = "cache-" + location.pathname;  // Segregate caching by worker location
const seconds = 1000 /*ms*/;

// There should be a Promise.delay like this but it's trivial to define.
const delay = (ms, val) => new Promise(resolve => setTimeout(() => resolve(val), ms));

onfetch = event => event.respondWith(handleFetch(event));

async function handleFetch(event) {
  let request = event.request;

  // Use only our specific cache. Most service worker examples match from the
  // domain-wide cache, "caches.match(...)", which seems like a bad idea to me.
  // Surely it's better to have each app manage its own cache in peace?
  // This is particularly useful when you serve test and production versions of
  // the app from the same origin.
  let cache = await caches.open(cacheName);
  let cacheResponse = await cache.match(request);

  // Issue a fetch request even if we have a cached response.
  let fetchResult = doFetch();
  async function doFetch() {
    try {
      let fetchResponse = await fetch(request.clone(), { cache: "no-cache" });
      if (fetchResponse.ok) {
        cache.put(request, fetchResponse.clone());
        return fetchResponse;
      }  
      if (cacheResponse) return cacheResponse;
      return fetchResponse;
    } catch (error) {
      if (cacheResponse) return cacheResponse;
      throw error;
    }
  }

  if (!cacheResponse) return fetchResult;

  // Resolve with the fetch result or the cache response delayed for a moment, whichever is first.
  // If navigator.onLine is false, the request will have failed immediately and we will have
  // already returned the cached response so this is not likely to happen often.
  // There's no point even checking navigator.onLine.
  return Promise.any([fetchResult, delay(2 * seconds, cacheResponse)]);
}

//
// Prefetches
//

let prefetchURLs = [
  "demo.scm",
];

onactivate = event => handleActivate(event);

async function handleActivate(event) {
  // The idea here is to issue prefetch requests one at a time at a leisurely pace
  // to keep from competing for network and other resources. It makes no sense
  // to compete with the app while it launches.
  let cache = await caches.open(cacheName);

  for (let url of prefetchURLs) {
    await delay(5 * seconds);
    let request = new Request(url);
    try {
      let fetchResponse = await fetch(request, { cache: "no-cache" });
      if (fetchResponse.ok)
        cache.put(request, fetchResponse);
    } catch (_) {
      // If it fails, it fails. But we persevere.
    }
  }
}
