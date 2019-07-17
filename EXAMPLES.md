# Example EPP commands

Can be used for local testing
```
Login
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<epp xmlns="https://epp.tld.ee/schema/epp-ee-1.0.xsd">
  <command>
    <login>
<clID>test_bestnames</clID>
<pw>testtest</pw>
<options>
  <version>1.0</version>
  <lang>en</lang>
</options>
<svcs>
  <objURI>https://epp.tld.ee/schema/domain-eis-1.0.xsd</objURI>
  <objURI>https://epp.tld.ee/schema/contact-ee-1.1.xsd</objURI>
  <objURI>urn:ietf:params:xml:ns:host-1.0</objURI>
  <objURI>urn:ietf:params:xml:ns:keyrelay-1.0</objURI>
</svcs>
    </login>
  </command>
</epp>

Logout without any parameters
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<epp xmlns="https://epp.tld.ee/schema/epp-ee-1.0.xsd" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="urn:ietf:params:xml:ns:epp-1.0 epp-1.0.xsd">
  <command>
    <logout/>
  </command>
</epp>

Logout
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<epp xmlns="https://epp.tld.ee/schema/epp-ee-1.0.xsd">
  <command>
    <logout>
<clID>test_bestnames</clID>
<pw>testtest</pw>
<options>
  <version>1.0</version>
  <lang>en</lang>
</options>
<svcs>
  <objURI>https://epp.tld.ee/schema/domain-eis-1.0.xsd</objURI>
  <objURI>https://epp.tld.ee/schema/contact-ee-1.1.xsd</objURI>
  <objURI>urn:ietf:params:xml:ns:host-1.0</objURI>
  <objURI>urn:ietf:params:xml:ns:keyrelay-1.0</objURI>
</svcs>
    </logout>
  </command>
</epp>

Poll
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<epp xmlns="https://epp.tld.ee/schema/epp-ee-1.0.xsd">
  <command>
    <poll op="req"/>
    <clTRID>foo bar baz</clTRID>
  </command>
</epp>

Check
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<epp xmlns="https://epp.tld.ee/schema/epp-ee-1.0.xsd">
  <command>
    <check>
<domain:check xmlns:domain="https://epp.tld.ee/schema/domain-eis-1.0.xsd">
  <domain:name>airport.test</domain:name>
</domain:check>
    </check>
    <clTRID>Some some some</clTRID>
  </command>
</epp>
```
