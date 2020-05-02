(ns colt.core
  (:refer-clojure :exclude [send])
  (:import (java.net.http HttpClient HttpClient$Redirect HttpRequest HttpRequest$BodyPublishers HttpResponse HttpResponse$BodyHandlers)
           (java.net URI)
           (java.time Duration)
           (java.util.concurrent CompletableFuture)
           (java.util.function Function)))

(defn- ->body-publisher* [x]
  (if x
    (condp #(= %1 (class %2)) x
      java.io.InputStream (HttpRequest$BodyPublishers/ofInputStream x)
      java.lang.String    (HttpRequest$BodyPublishers/ofString x)
      #_otherwise         (throw (IllegalStateException. "unsupported body type")))
    (HttpRequest$BodyPublishers/noBody)))

(defn send
  "Send a request, async"
  [{:keys [body uri method follow-redirects connect-timeout request-timeout pipeline-executor -http-client -http-request]}]
  (-> (.sendAsync
        (or
          -http-client
          (cond-> (HttpClient/newBuilder)
            follow-redirects   (.followRedirects
                                 (case (keyword follow-redirects)
                                   :never  HttpClient$Redirect/NEVER
                                   :always HttpClient$Redirect/ALWAYS
                                   :normal HttpClient$Redirect/NORMAL))
            connect-timeout    (.connectTimeout (Duration/ofMillis connect-timeout))
            pipeline-executor  (.executor pipeline-executor)
            :_                 (.build)))
        (or
          -http-request
          (cond-> (HttpRequest/newBuilder)
            method          (.method (name method) (->body-publisher* body))
            uri             (.uri (URI/create uri))
            request-timeout (.timeout request-timeout)
            :_              (.build)))
        (HttpResponse$BodyHandlers/ofInputStream))
      ^CompletableFuture
      (#(.thenApplyAsync
          %
          (reify Function
            (apply [_ response]
              (let [response* ^HttpResponse response]
                #:colt.response{:version (.toString (.version response*))
                                :body    (.body response*)
                                :status  (.statusCode response*)
                                :headers (.map (.headers response*))})))
          (or
            pipeline-executor
            (.defaultExecutor %))))
      #_(#(.exceptionallyAsync
            %
            (reify Function
              (apply [_ error]
                {:error error}))
            (or
              pipeline-executor
              (.defaultExecutor %))))))

(comment
  (try
    @(send {:uri    "http://wp.pl"
            :follow-redirects :normal
            :method :get
            :connect-timeout 1})
    (catch Exception e
      (println e))))