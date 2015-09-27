(ns wat.handlers-html)

(defn workspace-body []
  (str

"
<head>
 <title>WAT|Workspace</title>
</head>
<body>
<div class=\"translate\">
 <form method=\"post\" action=\"get-text-to-translate\">
  <h2>Get text to translate</h2>
  <p><input type=\"number\" name=\"chunk-size\" required=\"required\" value=\"2000\" ></p>
  <p><input type=\"text\" name=\"project-name\" required=\"required\" placeholder= \"Project name\" >
     <input type=\"submit\" name=\"get-chunk\" value=\"Get!\"></p>
 </form>
====================================
 <form method=\"post\" action=\"get-text-to-redact\">
  <h2>Get text to redact</h2>
  <p><input type=\"number\" name=\"chunk-size\" required=\"required\" value=\"2000\" ></p>
  <p><input type=\"text\" name=\"project-name\" required=\"required\" placeholder= \"Project name\" >
     <input type=\"submit\" name=\"submit\" value=\"Redact translated\">
     <input type=\"submit\" name=\"submit\" value=\"Redact redacted\"></p>
 </form>
====================================
 <form action=\"return-text\" method=\"post\" enctype=\"multipart/form-data\">
  <h2>Return text</h2>
  <p><input name=\"file\" type=\"file\" required=\"required\" /></p>
  <p><input type=\"text\" name=\"project-name\" required=\"required\" placeholder= \"Project name\" ></p>
  <p><input type=\"submit\" name=\"submit\" value=\"Submit as translated\" />
     <input type=\"submit\" name=\"submit\" value=\"Submit as redacted\" /></p>
 </form>
==================================== <br> <br>
 <form action=\"dashboard\" method=\"get\">
  <input type=\"submit\" name=\"submit\" value=\"Admin dashboard\"/>
 </form>
 <form action=\"logout\" method=\"get\">
  <input type=\"submit\" name=\"submit\" value=\"Logout\"/>
 </form>
</div>
</body>"))

(defn dashboard-body []
  (str
"
<head>
 <title>WAT|Dashboard</title>
</head>
<body>
<div class=\"translate\">
 <form method=\"post\" action=\"get-line-by-id\">
  <h2>Get line by id</h2>
  <p><input type=\"text\" name=\"line-id\" required=\"required\" placeholder=\"id\" ></p>
  <p><input type=\"text\" name=\"project-name\" required=\"required\" placeholder= \"Project name\" >
     <input type=\"submit\" name=\"submit\" value=\"Get!\"></p>
 </form>
====================================
 <form method=\"post\" action=\"search-string\">
  <h2>Search in lines</h2>
  <p><input type=\"text\" name=\"string-to-search\" required=\"required\" placeholder=\"String\" ></p>
  <p><input type=\"text\" name=\"line-attribute\" required=\"required\" placeholder=\"line/<attribute name>\" ></p>
  <p><input type=\"text\" name=\"project-name\" required=\"required\" placeholder= \"Project name\" >
     <input type=\"submit\" name=\"submit\" value=\"Search\"></p>
 </form>
==================================== <br> <br>
 <form action=\"workspace\" method=\"get\">
  <input type=\"submit\" name=\"submit\" value=\"Back to workspace\"/>
 </form>
 <form action=\"logout\" method=\"get\">
  <input type=\"submit\" name=\"submit\" value=\"Logout\"/>
 </form>
</div>
</body>"))
