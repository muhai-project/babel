PGDMP     8    &                {           geography.db    15.3    15.3     "           0    0    ENCODING    ENCODING        SET client_encoding = 'UTF8';
                      false            #           0    0 
   STDSTRINGS 
   STDSTRINGS     (   SET standard_conforming_strings = 'on';
                      false            $           0    0 
   SEARCHPATH 
   SEARCHPATH     8   SELECT pg_catalog.set_config('search_path', '', false);
                      false            %           1262    16451    geography.db    DATABASE     p   CREATE DATABASE "geography.db" WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE_PROVIDER = libc LOCALE = 'C';
    DROP DATABASE "geography.db";
                postgres    false            �            1259    16515    border_info    TABLE     J   CREATE TABLE public.border_info (
    state_name text,
    border text
);
    DROP TABLE public.border_info;
       public         heap    postgres    false            �            1259    16520    city    TABLE     �   CREATE TABLE public.city (
    city_name text,
    population integer,
    country_name character varying(3) DEFAULT ''::character varying NOT NULL,
    state_name text
);
    DROP TABLE public.city;
       public         heap    postgres    false            �            1259    16526    highlow    TABLE     �   CREATE TABLE public.highlow (
    state_name text,
    highest_elevation text,
    lowest_point text,
    highest_point text,
    lowest_elevation text
);
    DROP TABLE public.highlow;
       public         heap    postgres    false            �            1259    16531    lake    TABLE     �   CREATE TABLE public.lake (
    lake_name text,
    area double precision,
    country_name character varying(3) DEFAULT ''::character varying NOT NULL,
    state_name text
);
    DROP TABLE public.lake;
       public         heap    postgres    false            �            1259    16537    mountain    TABLE     �   CREATE TABLE public.mountain (
    mountain_name text,
    mountain_altitude integer,
    country_name character varying(3) DEFAULT ''::character varying NOT NULL,
    state_name text
);
    DROP TABLE public.mountain;
       public         heap    postgres    false            �            1259    16543    river    TABLE     �   CREATE TABLE public.river (
    river_name text,
    length integer,
    country_name character varying(3) DEFAULT ''::character varying NOT NULL,
    traverse text
);
    DROP TABLE public.river;
       public         heap    postgres    false            �            1259    16549    state    TABLE     �   CREATE TABLE public.state (
    state_name text,
    population integer,
    area double precision,
    country_name character varying(3) DEFAULT ''::character varying NOT NULL,
    capital text,
    density double precision
);
    DROP TABLE public.state;
       public         heap    postgres    false                      0    16515    border_info 
   TABLE DATA           9   COPY public.border_info (state_name, border) FROM stdin;
    public          postgres    false    214   -                 0    16520    city 
   TABLE DATA           O   COPY public.city (city_name, population, country_name, state_name) FROM stdin;
    public          postgres    false    215                    0    16526    highlow 
   TABLE DATA           o   COPY public.highlow (state_name, highest_elevation, lowest_point, highest_point, lowest_elevation) FROM stdin;
    public          postgres    false    216   9'                 0    16531    lake 
   TABLE DATA           I   COPY public.lake (lake_name, area, country_name, state_name) FROM stdin;
    public          postgres    false    217   �+                 0    16537    mountain 
   TABLE DATA           ^   COPY public.mountain (mountain_name, mountain_altitude, country_name, state_name) FROM stdin;
    public          postgres    false    218   @-                 0    16543    river 
   TABLE DATA           K   COPY public.river (river_name, length, country_name, traverse) FROM stdin;
    public          postgres    false    219   =/                 0    16549    state 
   TABLE DATA           ]   COPY public.state (state_name, population, area, country_name, capital, density) FROM stdin;
    public          postgres    false    220   73          �  x�}�[�� E��Ud����:fbC
��3�^��Hj��#��p%�f3_f7�a��)Y;�N.6ċ3��[���zw)�����&�����~�b	y�9 o�������b6��/��Ôe���dR]'ܣc"b��8��pw�o���X��fְ���B���%�	�c�i���W4�*H[�5/�cp`o��z����%�|���1�&%���d�#�����=��Ve�W���l7�4�N�|��=L�	��ɾ��&��g�����O9��������UV�^��Hwo͝O>�c=-&��y���09��8�&�Zôw4Eg�T=���_�}�Ikr��P�K�ӥ|��	�����j�q_�/&09
�<������.������d.���e��0M� ��RE��U�S?�k��'z��:&Xר	3�n�i�q�o��	_.��h�(��d1a�J�n�o������?ڙL���R�	��>҅[�!��E����D9�0a-?l,�2��=A�5��࿏����2�V�q?��%�̗�_Փ���B�"ǀ �@t�O�L���ȥ^��Mw��C�]�ky�iB=�n
���F�в�Bb�*j���U�R<������<�T{c��m�P~��˂!S����2.R�V��i�`+x�Y�2OŻ����M�7�ѡ���ۥ?�+�����_�RG���J��]+BVE��n��� GA�5B�408�YV~tҲ����Е0��"���[VV_^��sSM�~�b��j�6Ԯ�цV2��.l��]�p-Uz
*��RA�sv���lF8#F�Ƹ��Ą�&�B�#��՗�|\]�~�4M����W�V5v�
��M����^�����)�o�1&���Z�#��;�^���;�?7j�.:�	�И�R�i\�_@\��[C�b|9�ң�4�MZ�[�H���8�h����<�� E*E            x�uZ[��:��V��=B{�T�ڀh��]�D
	���s��zfFF��י�vW]�UE��dU�ZuU����մ:�Ҵ��駛��{%L���z��i�1-�eE��N�X��Z<�{P���:uÛ�(r�񡾆�ս�MD%EZ,�3�ڞ��G�'y��\�N�Ƴ���7t7`���L�[��Fa겔U����N����*�l�f�Z}q�~`�J2���G5~}[7]��L�D��*�h�~��Q�:|h�����n��d�,+���N��{�)���}�I%8ϫO���.,&)i+�Om��Q'e&�";­�o��V�=�K����z��op2���v���d�LpQ�cի�60H&svB��{�ʪ,=M3��H�fs�n�;��h����'Å;L�D3���0�`�Ry�Wb�L��a�>@�,N}O��������o��_n���I����Ct|W�z�4�)�*!Og4�Q�#Ep*N��8]���X���l�/�/��<�����Ny+OG1�}��񻔖%;�U=pT�F��?/ϖP۾�~�\���<0��yv��{;9�i�'K���im�Ȃ�����דV.s^�>X��Jd&��nÎ&�g߅Q�L�l��i�cS+��q|nQ�ʡ�驋�Ln/p��}���󦂇��}$U!�x��U���i��}n��ǌ�ϱ���s��U�������Ue�ٱk;��˫,�l��0+��y�ʍ�}p���
��gG~�~/��A4��iZ��V�����:��n�&�����x]p"/�3�NCOw�'}s��� �|��K�&m���`Lg.~�B�9�Z#����̕��oQ�g�l��=���c�Ws�1����S�?8�W6�a�Wsq�Ot,P-�B)"Щ�q� E���hg��v.) Oݫr���B��]��`�ˀ����؀Si���Cx�%?-b��zϫ3���@:a�>e^�� ��oL�:��q0W
���;���L/�τW%?����[H�W�<����a~\�1؋�hyezV �;�I!�<�mA�����\����!sg�)c#5;�B"�G���j�0�*=�ì�-�#VV�K��p&U���ڹm��Hn�!���d�1�yvxՙ��<��A����'S���}�a+�	�9\�
΀P_��<5xhه��<� ��RD�3�m�0>VA6C~If;��u��1jl�4��S,���nN��iȜ$�	��P-�-��6R0XO����>M�-z.i�(�n���������#��M�M�;wW��U?C�����Gmx�Q�I���w ���@�0N�\���7�$�+�ͮ�7�VͰ�EM�<�安u$�a�"n����Wp��n���%T���_�[4B�)�����q�H�/���`):�~�ߡ��[bu�ϛѠ�.2_�����5Hj�W��.��I��G#���Q�KC�v��ݼ`���!lK�8l���G�HB�,��u�H�� �B!�"a�;�n{���`��
��4�K?�b2��>,V3�AT%s�o��5�^4�V��޼Cb�4�,�yCĄ�_�N�����4�F���{h��(��s��]+Di�`��C��!�d�G����	|#�-��Ʈ��U���懾)8�(��G�ڐ�Ee�|�;oo��,�jSk
�������=�V��qk�ae�L���Ǜ(	JV�A����t�@ԥ)R����|q��z���"�-w(r?�<�b\Cx�7.�[�8��ȝ+\��Id�1[/�`g!��H7�5�(0�}�`�5B^���2�F8�I[1��Q��P�D�zT8G��;B��S�i|������Q�h����ο��Fʴ���Wk���q{�k��g�GyH��?s j�	5���2Ú�t��~�$�& �瞛�X��3�J�l�}���?H�֏�W�C`��Ud�(Ee0?���s�C����7������i�@�����E� ����4B���r��7ڪo��θ���	}���Q� yGXۻ�ѓ6G�͎���YX�8@4Y_U*�Q��b��t��T�R���U�5,��I9���
|�6hf�I@+U��UOw=z������8�g�b"B�<bg@X���g(0���Ɏ^!اF^����7%�;MM�
I�DB"dZ\](X�5:�|#,_|u,��V�ڧF��&�w6}7,D�^�������W�����FQ�yq���q�N�:v��4P��TX+s�Z���`H��C���1 1
I�o��Ie)�d8L��-�\"�Hp��.�P��t>(&$n+��n��E�i)��]���[d|רȐ@�ô�s.��-�4�<����=������7ڿ�J�����VE�����W�Ǩ`���I� ݨ\B�{5
�[=}%� ������D`���C1��!F�`�0Y/�4nM{O�$+�ËξTK�D�V����$"���j���zc
8�%�"�z _�,=!k8��V���mB�uQ$��F2b~xz�	��<�R5���i,���wB:�U��"�n�0�~~��N��,���.E4�^�T`�x��H����Y�0���w;;�����=�#�<g��E#�h<s0�R!0�;��tY2�}��ј@Bo���zYx#�Z�����sZM"e�T)����퐀%9���W�o��F�&��}c؆hhJy�2P|���ˡ��4�&��n㖃��D"F5�n@��G=+�H*�')�O�!lT+��Q�����dY���-�:{��2d�X��_EQ���Py�����yI�G�r�6����J�����<0]��R��8-���ic��۾ݘ1T$�ӋwH�wҤD����^T�@�KC�`v�iZ=YJZIN/���R�`�yp�$�	d�B�Q����^���#��A.>���߰Yd�{��C@�[�Ś�y�7Z+6������ٱ����v����f�0���y��Ű���<�M
,~cv@�C������j���_�W$i�]{O��TC�.š_��+��Ρ�ה+n2�w&-�E�����L���H���ے4=�@��r�������6"8����9�R�Zn8٧O����h��me�u߮��*�K��>4@��_K�F�q�!��7��_+�Qr;P�Fjl����d˪���w-�Q��. �aHM&ɩ&��d[ݐ�e�ߏ��Q�B2-6��Zr�<�v��!�F���"��2�Z�+O�� ��Ȗc��?�b3�gy�6l��G��NôKѓ����Q���Ln�?h�}�TR�����v�(+���
J���7��}���"1��`�����e?�X(w��
Ԋ��;*
UiHaWdMs�3�PN�l�i�4X ��b,�\�,ߵ@ƪ�-~B
�U%$tA?���/}ᔾ�{O�,���wM�Ϩ�d��麤_l!]ˣ�c���������n�IiXܫ]Ȋ���U���J[}�&�Y�)�P�.Ts��W�����b���i�a����B$��"��!�G�"��a�&K��A�a�gLn�����6�bF��C�<M�H���Lv0m�R�d2�����9�*g"����*���l�Х�rV��x�Ut�6��6-��\����KVY��C��h�D�
�f�i����}��
�p��U���[�(>�	��GoC1҄7�ULP���ș�C����e��Y�_5ҕ9D,�Z�{yNW��3�����J7�n����� � �`������@�0pj^�XM]���3��;ٰI�f#�}������[`�Qc�i�� u�a�,��,�N*�Sv!~a� W���"V��qA6������-ҏJ����T���J��}���H����R'�#�(������-�v������v�p� ᮨ�R�����-"ϊ�B��qI�w�3`���{;���2���)XQnw��o�Hy�]��j��7���Z�gC�m �m�jO����vO����2�q�5���͎��շt�Guԥ윧�&4O���,XH��C�?� ��7do�ȔET�N���#ɐ�S�1�f���p   ����&��!��PZ�����^��8�'?L%����>��3��c�D�Aco`��a�x�ʨh���m�_�4�32$�v�C=�8�־���ZH��0���D�����ߗ��b�����izD'8`�|��i^)��A:B	۴A�!��B�!�{�~�H�_!�@��5������I߇Rq�SK���+V�ޠ�7��U8�7����Ey�zC����ӥ�����) �:NH�t�&��|���#�G��˾������� �/��         {  x�uV�n�:|&�B?�BwY�r^V�ZbE�I�u��e9q����pwf6dh��DW�b��9s�l��Z9�f����m6��"�d(,$ڢ�Ņ�>k�9�d���j���=�z��Yթ)�r�y]�������2{���´��@�B6P��n#5�HG�J�і?��Td��y�I�uq#S��+�����~�� ��ų۳��f`E��rֲ�ZmQte#��S��!;f��s�:6��Ѝ<��jE�Ə�(���U\��!z�b�4pm�G�rqѭ�T��$�����_�˿���΢ ��O���*���=v�B*�dF�����K�}[(m���	Q�<���^K�t~���x��NQu�X1��s��#Z���}���N�5������gOvA�,�����$���h�⛺4��H��\ԭ<�T�U!3�	��k�a����˼���厌����`H-���NҸM�~�˷��uh+F�Z0����\��h��}%8E��]c+�{"Zbb�e��1�W-�� �-p�xs�~�z���8�ԒR46�6��Ie���S��9��]�@!�ȋGd�.v^0M�U��J�LN�u�n(�,�N&�3��h�\��3K�Wˊ��Y��u�Hr�31���9H������#?h�Խ��X��E5�@�[x&���l�{a[ٴȹ�V�św�dXѣAQW2-����$��辟��a'��NI���S���	X�崍G�1�:�<��`��9�`+N{q���(�tO�;�m��DS�0��NE�Au=T�x��#����#g�%Q��wT�	%��l�bdQ��L�uW��\a^Je3��,�[L��h�V���ǁĩ���Vu���wsyak��\)�����Ӡ�����0�\�ٍ|8<��e X�^�����N�ϙ��/�$K�9����w̭���AO,�N�0Iܬ�TO��!��e�����x�ѭ2!�7��l��]�V��Q?�\G԰I10�����̋�!�����ėIuX��*qޤg=�d�Y3�
v~ոAi�0�X�w�/��u.�]�G��"�������|����7��b݀j�M\� �M�<��_��
v����nJװ�����M���[ˈ�1��n�?���PA�         l  x�m�Q�� ���Sp��#�]����qJd,Фr��$��J�:4=ݰc�<��V%��Օ���`Lm����@㼎P��8z���ӈn�#!4��EZt�K��p�KS�@FJ�J�6o��I�����7�P֧�v��p<d�c��f�˶��=�q�d��:יK��\�7�/��5Hm�����Fp.����|��!��i2vd�{���r8��^/�H�R4&;�K����i��O��)eG�a�,l{��-��t0�����y�c*&���C?%�j��v����G��iv�B��X&'K>����tw���-.z�pz*y�ߦ����;�m���[����-�+�ʲ�ؿ/��7-�5         �  x�m�]�� ��u���
��e^"��#5��q�t�[�z�G��M���
����m2P�Vȝ�(PIpY���N>��mְ2����)yT �q�:����ɊZ�tP����
�yT֙�/Ÿyoѹ�����ha���]�܉X6+��6��	��~	Ym��l�����뢆�qS�� ��7i|(㯴x��u�h$Ģ^Gkv��7��WGp��g��*��/�k%�U�(I^jI����p�����T���/��%۟p0�O��[�}��sG��P�h=���MEk5����W(�o]���uN�MLFI��:
���S[W�h�uy<a�m��V14�k2��?%e�`c�c2[8\p(i	�������6E���t������x��N���+��3��F+q�O\�y�WA0KzQ]���<-ق��/e��,4�tv��8�m�� ���٭�쥾J�P�D}�sզ�e͕�xaAjn��;�E�d�>�<��׬�         �  x���ٲ� ���S�S&��27�(#B
�8��O#	K�,U����v7?81�����HVU5�5��	A�4p�bxa��B3�L.o:2Ι�L��&�Y�8�0s;�qj�bMi�Ah�b�o���4�f[.޶@��|�-��!���#�.ƿ_adoPzU���J^����K�$�,���P"h60DdP���D�O�b��>�Nr`�������U��B�Pm�O�z2[+��Dwϒ�E�W���hG�ev�|I&����n�O,8G�#�ANશ8�B}���#�
�c����.�����s^�^Q�%!�C�vh+�����!eQ��nz`�F[|R��(�@kh�YSct�ŋv�(�%yq���R�!qJ3��U��!;�g�p���ؤ{�e����[~x�;�XǻF��h#�*�Rg��:̒/JE{)� �%z�%i0�!R���J/�=�x����%�,�M���)��,}v���۪��X��p���w;�4{Pbm��8n�6��%�������M\yK��qK���W����F��$�#��ӕm�v�����&eո��0��;��ɤW�2J�Է�|5��H�H~�8[�2 /��C�{z�ӓ7�+%����������R��������[)�r�x8�	j����(t<8��@�J�KZh�������>H������R�1�-���k�C ���o��)#���1D�WG���"�/�/mQ�j;��i�f9���e�}#�K�=:�H�8�:��я��0wm4o.n#��T�A�*O_��Z;L�q( e�����0r�7 ��5���	��u��8��㍌�z��ڼ�:$�t�KG)Sw�|ԗ� $^�e����=�-(�z�-Vd�x0ۯ6G�g_Or\�����.d���e������Q�m�%��-�s�S$m��2��}E,��.����O�/�j+mC��ƀX�XWY�f���p4G<��f<�F�]���N��o18�         �  x�mV[�9�n��'h����]�Gckl��я�xO�%�=H���)�,V��q�>)����qx�y��GZ^��QS ヶ�^��S\�q0�|�oԯ}Nq�h]��Xm��tq���9�U���z�J���A��E����-u���A)o[�Z�QS޶)��r�F�����'%��N��?�2gT�CՒ쏒k</��d��I���r�q.SY���А��K��҂1F�7�X�O��9��|޷A�t�]�:���\K~D�Z��ƉK��w\xlU�4��*�R ��Z�\jVJ�K^�%��S�<����1�~��n�;��<_�2�[|b�|�Cp�i�����)�⺦2h�1���g��5�功g,�MTi��Mq���jt,�2�xEN+q�<9��y�q�9(3��vTNi�u�Z�
�v+C0����G�+��Q;m������Ӕg|��Qm��ח�\0�gN�d��`T�Y:($�<_r��@��*����Y���L�eg��Ys�"�o�)����%��G�sZa��i�	��|+��N�Wl+�)?:evQ�Z��]����@�K��������ym�%ے�;��!�e�������N����'�hT*n�hx��x��+V��L
Z�v����/(E�m���|ph���w���)�VXﶯi�@�Ӯ���{�kի��X��HXS���[��b��2�#@0�]�=je���'Pp�Z��T����H����4@�*h���pZ�u���<(V�o&��}��R#tJ�9�X��+���{,����gZ ;��D��O"7�F�AM�m��2��-M��u�-��Gr $�����b�ض�r�	��\��~��r0�a�@}E� ��{Z<����p�6�
p�¾O��x"S�T����б�n!����W^q�0yj˽M���ة��K'CL�P��;��Z6������\�>!X9�7��}1B��r��Є#���נٍ�Q���*f��v��me��.1�U4����7��Lp�3�A��{���2˪.����F��/8��j���6�-c8�{x:V��|GbXg�$��A.,�A]깖ݩ�y�a�W}Fc��WG��.�
��Sc�t�M����(��X(U��y}M_��P"���T�m��Ǿ\[��
��Sl��r+�t�kK�`��T��/�+㌞Ԏ����
��]h��C�z�?�')G6]`��r`�]X�AlD���i�tqy�����Fl��D�~����2��8�_�5Uk@�M5{����NF�f?e�M���uˈ��X���މ}������(�(z�NS���>���^r]=�3��@�\����?ϴg�0[����ՀP!ɾ2nw] kc+�-���� _`0�9p?Rw�������B�^�g}n��_`�bl�i�N?ᬦle��z�d���#�p���Pt��"D�CK�v`��>că����#���K�"�_�Q��P���w �ҫ�u0H�d�SI̊�?��?�~�e     