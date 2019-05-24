-module(certs).

-include_lib("public_key/include/public_key.hrl").

-export([read_pem_certificate/1, get_subject_from_otp_certificate/1,
         get_common_name_from_subject/1, certificate_to_pem/1,
         read_der_certificate/1]).

%% Read certificate from the wire and return back an otp type record
read_der_certificate(Der) ->
    Certificate = public_key:pkix_decode_cert(Der, otp),
    Certificate.

%% Take an otp record and convert it into a string that can be set into HTTP
%% header SSL_CLIENT_CERT.
certificate_to_pem(Certificate) ->
    PemEntry = {'Certificate', Certificate, not_encrypted},
    PemString = public_key:pem_encode([PemEntry]),
    binary:replace(PemString, <<"\n">>, <<" ">>, [global]).

%% Read only a specific type of certificate, otherwise fail.
get_subject_from_otp_certificate(Certificate) when is_record(Certificate, 'OTPCertificate') ->
    Subject = Certificate#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.subject,
    Subject.

%% Take a subject rdnSequence that can be set into
%% HTTP header SSL_CLIENT_S_DN_CN.
get_common_name_from_subject(Subject) ->
    CommonName = ?'id-at-commonName',
    {_Type, Field} = get_field_from_subject(Subject, CommonName),
    Field.

%% Only used for local development test, is not required for the application.
read_pem_certificate(PathToCert) ->
    {ok, PemBin} = file:read_file(PathToCert),
    PemEntries = public_key:pem_decode(PemBin),
    {value, CertEntry} = lists:keysearch('Certificate', 1, PemEntries),
    {_, DerCert, _} = CertEntry,
    Decoded = public_key:pkix_decode_cert(DerCert, otp),
    Decoded.

get_field_from_subject({rdnSequence, Attributes}, Field) ->
    FlatList = lists:flatten(Attributes),
    ValidAttrs = lists:filter(fun (X) ->
                                      X#'AttributeTypeAndValue'.type =:= Field
                              end, FlatList),
    Attribute = lists:last(ValidAttrs),
    Attribute#'AttributeTypeAndValue'.value.
