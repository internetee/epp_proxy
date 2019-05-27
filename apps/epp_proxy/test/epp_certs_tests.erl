-module(epp_certs_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

-define(exampleCertFile, "fixtures/epp-proxy-test.crt.pem").

wder_certificate_test() ->
    PemEntries = public_key:pem_decode(certificate_pem()),
    {value, CertEntry} = lists:keysearch('Certificate', 1, PemEntries),
    {_, DerCert, _} = CertEntry,
    Certificate = epp_certs:der_certificate(DerCert),
    ?assert(is_record(Certificate, 'OTPCertificate')).

subject_from_otp_certificate_test() ->
    Certificate = test_certificate(),
    Subject = epp_certs:subject_from_otp_certificate(Certificate),
    {rdnSequence, _ListOfAttributes} = Subject.

common_name_from_subject_test() ->
    Certificate = test_certificate(),
    Subject = epp_certs:subject_from_otp_certificate(Certificate),
    CommonName = epp_certs:common_name_from_subject(Subject),
    ?assertEqual(<<"Epp Proxy Test">>, CommonName).

test_certificate() ->
    PemEntries = public_key:pem_decode(certificate_pem()),
    {value, CertEntry} = lists:keysearch('Certificate', 1, PemEntries),
    {_, DerCert, _} = CertEntry,
    Decoded = public_key:pkix_decode_cert(DerCert, otp),
    Decoded.

%% Contents from fixtures/epp-proxy-test.crt.pem file
certificate_pem() ->
    <<"-----BEGIN CERTIFICATE-----
MIIGWzCCBEOgAwIBAgICEAEwDQYJKoZIhvcNAQELBQAwgaoxCzAJBgNVBAYTAkVF
MREwDwYDVQQIDAhIYXJqdW1hYTEQMA4GA1UEBwwHVGFsbGlubjEjMCEGA1UECgwa
RWVzdGkgSW50ZXJuZXRpIFNpaHRhc3V0dXMxLzAtBgNVBAMMJk1hY2llaidzIGRl
dmVsb3BtZW50IGNlcnRpZmljYXRlIChFSVMpMSAwHgYJKoZIhvcNAQkBFhFoZWxs
b0BpbnRlcm5ldC5lZTAeFw0xOTA1MjMxMjUwMzFaFw0yMDA1MjIxMjUwMzFaMIGp
MQswCQYDVQQGEwJFRTERMA8GA1UECAwISGFyanVtYWExIzAhBgNVBAoMGkVlc3Rp
IEludGVybmV0aSBTaWh0YXN1dHVzMSMwIQYDVQQLDBpFUFAgUHJveHkgdGVzdCBj
ZXJ0aWZpY2F0ZTEXMBUGA1UEAwwORXBwIFByb3h5IFRlc3QxJDAiBgkqhkiG9w0B
CQEWFWVwcC1wcm94eUBpbnRlcm5ldC5lZTCCAiIwDQYJKoZIhvcNAQEBBQADggIP
ADCCAgoCggIBAMQalaRFIgknGeQ5iuoBju+cviOksq85azQRIwDaxwwjpFwmRwt2
smPa/0GMyG3/FBGNpUBtb+0ngVMCw5h4aUeWhs9Y9d1kMqaEgpdLm0hWpUIIqOMO
wQpHugPQGJiz75Y4tIUbdazODTvEChDNPZ7Ut8jVzsoRFIgPV4abcoqTqD0wNYZb
TQ0MzOulB2PsxzKrT9MzWkdyju3FYtDxXRGKP3vXC3YhN4EXLqoDzMU51NoYQcPc
Fk5j+1aQKXfNwJcLEvHGlHce9MIEakl5sJt+u8REmp43uy//DNa7mzACfu4xHAW+
heuH+xhEZAT40oxFi0Goa2rhMjz7hx5GM8KPFvJ0vlkR74pRnz1/XNqpx5PundYY
RK0swk+E+z3oFqClsjFiBVZQSPnYdGhid3qDaSqTsVO4G07rzh54SrBm7AsU8CQF
45OOngr1razw5jKnWH6qzNLFKcZBchOSmW31ZpTqfbol6UhThEM6qZr5Dsk+QZbd
kVZ5v3YZs72HL5Fxn/Ix5WOL886fSBOOHyg9rkFoypXZxfyZZehbhY3alszqJFbj
bJ4f+tt2+ObCbkUiN3huP+i+41l+eIBobtsaQhtdzp/44KWR5u4kk2OexO4oZPFN
futAsemo/sxU8E9KvjS02j2zIXTNyV/8Axp30LhHFQvey8QtNbhM2YClAgMBAAGj
gYkwgYYwCQYDVR0TBAIwADALBgNVHQ8EBAMCBeAwLAYJYIZIAYb4QgENBB8WHU9w
ZW5TU0wgR2VuZXJhdGVkIENlcnRpZmljYXRlMB0GA1UdDgQWBBSec3EaCPVAovnz
u01SwyEoc9Kt6zAfBgNVHSMEGDAWgBRF1wL+I61d2uBQ1467prsu2SYRGTANBgkq
hkiG9w0BAQsFAAOCAgEAPks4fHoAvzjidw8aJB+nX1pDzbWEbeWOl4xvYizvCN/c
rWeFp6+BnaUuQK5lXTS1z9m2bf70sSFOFTl6JHbpYdU7xr5Qfn69gfdIqeC2k52g
R29oYYRQSjMxufHW9IMufJ/FL1PW6EIlahx01WWla/wg6EC3Bygvnz5oCsDaNTwO
eWKSFVwnE1l6kOoxSparvOdrKpbqvmhUGy8qCRwvLtF2CRQihYSHnnU18oe78REP
Hkyqfjr/GxHn8senKJcN9AQ4RjxMeH/X1wr7Z+0OTpQh61AMEzzyO9sHy9uvxQoN
FREhTs53T10Br5Ppx7hp9S6jmiYgL8wtJwJIWFowq87VfuyeqEKhFXqjybeAXIlv
o4dpUd3woW5rBWhmo6tVs04vNtD6LFktbgkRIV8PPeb1M4QkJ8eD6z7tJNEHVGmK
S0vlDrdTzERwJlhh52d7a6vPXGG1/S7Mji/xyyDuWClaoJXy5NVr8DnKb1WudAXH
nspSoh4YApK4oWTKxYWuhs7rshxFu5A2T03RzJbgsAA/9wrZ4BZz4OCtEVUYoIhV
Qv4f2eOKB8d//vYJWon4R6ukYDZZnYNGplmxl47rzcEam1K88IeaBLN1hirvb9G9
Xk+wDNgZwJmHtYeJSOZHjiQCl2YJCiVn4gbuDxjrEJvye4s6n8fdn4sv2kX06MU=
-----END CERTIFICATE-----">>.
