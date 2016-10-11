Oreilly free ebook address:
```
http://www.oreilly.com/data/free/
http://www.oreilly.com/business/free/
http://www.oreilly.com/iot/free/
http://www.oreilly.com/programming/free/
http://www.oreilly.com/security/free/
http://www.oreilly.com/web-platform/free/
http://www.oreilly.com/webops-perf/free/
http://www.oreilly.com/design/free/
```

get the pdf link:
```
grep -o "href.*csp" page_source_file | sed "s/href=\"//" | sed "s/free/free\/files/" | sed 's/csp/pdf/'
```
