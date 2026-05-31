structure(list(method = "POST", url = "/api/helloworld/logincheck?username=ABCDEF&password=1234abcd&username=ABCDEF&password=1234abcdNA",
    status_code = 200L, headers = structure(list(Date = "Sun, 31 May 2026 14:29:08 GMT",
        Server = "Generic Web Server", `Cross-Origin-Opener-Policy` = "same-origin-allow-popups",
        `X-XSS-Protection` = "1; mode=block", `X-Frame-Options` = "SAMEORIGIN",
        `Referrer-Policy` = "no-referrer", `Cross-Origin-Resource-Policy` = "same-origin",
        `Cross-Origin-Embedder-Policy` = "require-corp", `Strict-Transport-Security` = "max-age=31536000; includeSubDomains",
        `X-Content-Type-Options` = "nosniff", `Permissions-Policy` = "geolocation=(), camera=(), microphone=(), payment=(), usb=(), vr=(), fullscreen=(self), autoplay=(self)",
        `Content-Type` = "application/json;charset=UTF-8", `Content-Length` = "171",
        `Cache-Control` = "no-store, no-cache", Pragma = "no-cache"), class = "httr2_headers"),
    body = charToRaw("{\"Status\":\"Ein Fehler ist aufgetreten\",\"Username\":\"DE5256891X\"}"),
    timing = c(redirect = 0, namelookup = 0.000672, connect = 0.021788,
    pretransfer = 0.080575, starttransfer = 0.712569, total = 0.712594
    ), cache = new.env(parent = emptyenv())), class = "httr2_response")
