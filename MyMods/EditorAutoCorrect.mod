c?	= ": CARDINAL"
i?	= ": INTEGER"
li?	= ": LONGINT"
b?	= ": BOOLEAN"
lr?	= ": LONGREAL"
p?	= "PROCEDURE \c()"
pp?	= "PROCEDURE \c();\nBEGIN\n\i\n\oEND"
if?	= "IF \c THEN\n\i\n\oEND"
ife?	= "IF \c THEN\n\i\n\oELSE\n\i\n\oEND"
eif?	= "ELSIF \c THEN"
ifa?	= "IF (\c) AND () THEN\n\i\n\oEND"
ifo?	= "IF (\c) OR () THEN\n\i\n\oEND"
iifa?	= "IF (\c) AND\n   ()\n\b\b\bTHEN\nEND"
iifo?	= "IF (\c) OR\n   ()\n\b\b\bTHEN\nEND"
cc?	= "CASE \c OF\nELSE\nEND"
w?	= "WHILE \c DO\n\i\n\oEND"
wa?	= "WHILE (\c) AND () DO\n\i\n\oEND"
wwa?	= "WHILE (\c) AND\n      ()\n\b\b\b\b\b\bDO\n\i\n\oEND"
f?	= "FOR \c :=  TO  DO\n\i\n\oEND"
fi?	= "FOR i := \c TO  DO\n\i\n\oEND"
fj?	= "FOR j := \c TO  DO\n\i\n\oEND"
fk?	= "FOR k := \c TO  DO\n\i\n\oEND"
fx?	= "FOR x := \c TO  DO\n\i\n\oEND"
fy?	= "FOR y := \c TO  DO\n\i\n\oEND"
fz?	= "FOR z := \c TO  DO\n\i\n\oEND"
l?	= "LOOP\n\i\c\n\oEND"
r?	= "REPEAT\n\i\n\oUNTIL \c"
ro?	= "REPEAT\n\i\n\oUNTIL (\c) OR ()"
def?	= "DEFINITION MODULE \c;\nEND "
imp?	= "IMPLEMENTATION MODULE \c;\nEND "
uns?	= "UNSAFEGUARDED"
as?	= "ASSERT(\c)"
unref?	= "UNREFERENCED_PARAMETER(\c)"
asn?   = ":= "
op?    = ":= "
