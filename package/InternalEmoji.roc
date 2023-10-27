## WARNING This file is automatically generated. Do not edit it manually. ##
interface InternalEmoji
    exposes [EMOJI, fromCP]
    imports [InternalCP.{ CP, toU32 }]

EMOJI : [Pictographic, Base, Modifier, Presentation, Component, Emoji]

isPictographic : U32 -> Bool
isPictographic = \u32 -> (u32 == 169) || (u32 == 174) || (u32 == 8252) || (u32 == 8265) || (u32 == 8482) || (u32 == 8505) || (u32 >= 8596 && u32 <= 8601) || (u32 >= 8617 && u32 <= 8618) || (u32 >= 8986 && u32 <= 8987) || (u32 == 9000) || (u32 == 9096) || (u32 == 9167) || (u32 >= 9193 && u32 <= 9196) || (u32 >= 9197 && u32 <= 9198) || (u32 == 9199) || (u32 == 9200) || (u32 >= 9201 && u32 <= 9202) || (u32 == 9203) || (u32 >= 9208 && u32 <= 9210) || (u32 == 9410) || (u32 >= 9642 && u32 <= 9643) || (u32 == 9654) || (u32 == 9664) || (u32 >= 9723 && u32 <= 9726) || (u32 >= 9728 && u32 <= 9729) || (u32 >= 9730 && u32 <= 9731) || (u32 == 9732) || (u32 == 9733) || (u32 >= 9735 && u32 <= 9741) || (u32 == 9742) || (u32 >= 9743 && u32 <= 9744) || (u32 == 9745) || (u32 == 9746) || (u32 >= 9748 && u32 <= 9749) || (u32 >= 9750 && u32 <= 9751) || (u32 == 9752) || (u32 >= 9753 && u32 <= 9756) || (u32 == 9757) || (u32 >= 9758 && u32 <= 9759) || (u32 == 9760) || (u32 == 9761) || (u32 >= 9762 && u32 <= 9763) || (u32 >= 9764 && u32 <= 9765) || (u32 == 9766) || (u32 >= 9767 && u32 <= 9769) || (u32 == 9770) || (u32 >= 9771 && u32 <= 9773) || (u32 == 9774) || (u32 == 9775) || (u32 >= 9776 && u32 <= 9783) || (u32 >= 9784 && u32 <= 9785) || (u32 == 9786) || (u32 >= 9787 && u32 <= 9791) || (u32 == 9792) || (u32 == 9793) || (u32 == 9794) || (u32 >= 9795 && u32 <= 9799) || (u32 >= 9800 && u32 <= 9811) || (u32 >= 9812 && u32 <= 9822) || (u32 == 9823) || (u32 == 9824) || (u32 >= 9825 && u32 <= 9826) || (u32 == 9827) || (u32 == 9828) || (u32 >= 9829 && u32 <= 9830) || (u32 == 9831) || (u32 == 9832) || (u32 >= 9833 && u32 <= 9850) || (u32 == 9851) || (u32 >= 9852 && u32 <= 9853) || (u32 == 9854) || (u32 == 9855) || (u32 >= 9856 && u32 <= 9861) || (u32 >= 9872 && u32 <= 9873) || (u32 == 9874) || (u32 == 9875) || (u32 == 9876) || (u32 == 9877) || (u32 >= 9878 && u32 <= 9879) || (u32 == 9880) || (u32 == 9881) || (u32 == 9882) || (u32 >= 9883 && u32 <= 9884) || (u32 >= 9885 && u32 <= 9887) || (u32 >= 9888 && u32 <= 9889) || (u32 >= 9890 && u32 <= 9894) || (u32 == 9895) || (u32 >= 9896 && u32 <= 9897) || (u32 >= 9898 && u32 <= 9899) || (u32 >= 9900 && u32 <= 9903) || (u32 >= 9904 && u32 <= 9905) || (u32 >= 9906 && u32 <= 9916) || (u32 >= 9917 && u32 <= 9918) || (u32 >= 9919 && u32 <= 9923) || (u32 >= 9924 && u32 <= 9925) || (u32 >= 9926 && u32 <= 9927) || (u32 == 9928) || (u32 >= 9929 && u32 <= 9933) || (u32 == 9934) || (u32 == 9935) || (u32 == 9936) || (u32 == 9937) || (u32 == 9938) || (u32 == 9939) || (u32 == 9940) || (u32 >= 9941 && u32 <= 9960) || (u32 == 9961) || (u32 == 9962) || (u32 >= 9963 && u32 <= 9967) || (u32 >= 9968 && u32 <= 9969) || (u32 >= 9970 && u32 <= 9971) || (u32 == 9972) || (u32 == 9973) || (u32 == 9974) || (u32 >= 9975 && u32 <= 9977) || (u32 == 9978) || (u32 >= 9979 && u32 <= 9980) || (u32 == 9981) || (u32 >= 9982 && u32 <= 9985) || (u32 == 9986) || (u32 >= 9987 && u32 <= 9988) || (u32 == 9989) || (u32 >= 9992 && u32 <= 9996) || (u32 == 9997) || (u32 == 9998) || (u32 == 9999) || (u32 >= 10000 && u32 <= 10001) || (u32 == 10002) || (u32 == 10004) || (u32 == 10006) || (u32 == 10013) || (u32 == 10017) || (u32 == 10024) || (u32 >= 10035 && u32 <= 10036) || (u32 == 10052) || (u32 == 10055) || (u32 == 10060) || (u32 == 10062) || (u32 >= 10067 && u32 <= 10069) || (u32 == 10071) || (u32 == 10083) || (u32 == 10084) || (u32 >= 10085 && u32 <= 10087) || (u32 >= 10133 && u32 <= 10135) || (u32 == 10145) || (u32 == 10160) || (u32 == 10175) || (u32 >= 10548 && u32 <= 10549) || (u32 >= 11013 && u32 <= 11015) || (u32 >= 11035 && u32 <= 11036) || (u32 == 11088) || (u32 == 11093) || (u32 == 12336) || (u32 == 12349) || (u32 == 12951) || (u32 == 12953) || (u32 >= 126976 && u32 <= 126979) || (u32 == 126980) || (u32 >= 126981 && u32 <= 127182) || (u32 == 127183) || (u32 >= 127184 && u32 <= 127231) || (u32 >= 127245 && u32 <= 127247) || (u32 == 127279) || (u32 >= 127340 && u32 <= 127343) || (u32 >= 127344 && u32 <= 127345) || (u32 >= 127358 && u32 <= 127359) || (u32 == 127374) || (u32 >= 127377 && u32 <= 127386) || (u32 >= 127405 && u32 <= 127461) || (u32 >= 127489 && u32 <= 127490) || (u32 >= 127491 && u32 <= 127503) || (u32 == 127514) || (u32 == 127535) || (u32 >= 127538 && u32 <= 127546) || (u32 >= 127548 && u32 <= 127551) || (u32 >= 127561 && u32 <= 127567) || (u32 >= 127568 && u32 <= 127569) || (u32 >= 127570 && u32 <= 127743) || (u32 >= 127744 && u32 <= 127756) || (u32 >= 127757 && u32 <= 127758) || (u32 == 127759) || (u32 == 127760) || (u32 == 127761) || (u32 == 127762) || (u32 >= 127763 && u32 <= 127765) || (u32 >= 127766 && u32 <= 127768) || (u32 == 127769) || (u32 == 127770) || (u32 == 127771) || (u32 == 127772) || (u32 >= 127773 && u32 <= 127774) || (u32 >= 127775 && u32 <= 127776) || (u32 == 127777) || (u32 >= 127778 && u32 <= 127779) || (u32 >= 127780 && u32 <= 127788) || (u32 >= 127789 && u32 <= 127791) || (u32 >= 127792 && u32 <= 127793) || (u32 >= 127794 && u32 <= 127795) || (u32 >= 127796 && u32 <= 127797) || (u32 == 127798) || (u32 >= 127799 && u32 <= 127818) || (u32 == 127819) || (u32 >= 127820 && u32 <= 127823) || (u32 == 127824) || (u32 >= 127825 && u32 <= 127867) || (u32 == 127868) || (u32 == 127869) || (u32 >= 127870 && u32 <= 127871) || (u32 >= 127872 && u32 <= 127891) || (u32 >= 127892 && u32 <= 127893) || (u32 >= 127894 && u32 <= 127895) || (u32 == 127896) || (u32 >= 127897 && u32 <= 127899) || (u32 >= 127900 && u32 <= 127901) || (u32 >= 127902 && u32 <= 127903) || (u32 >= 127904 && u32 <= 127940) || (u32 == 127941) || (u32 == 127942) || (u32 == 127943) || (u32 == 127944) || (u32 == 127945) || (u32 == 127946) || (u32 >= 127947 && u32 <= 127950) || (u32 >= 127951 && u32 <= 127955) || (u32 >= 127956 && u32 <= 127967) || (u32 >= 127968 && u32 <= 127971) || (u32 == 127972) || (u32 >= 127973 && u32 <= 127984) || (u32 >= 127985 && u32 <= 127986) || (u32 == 127987) || (u32 == 127988) || (u32 == 127989) || (u32 == 127990) || (u32 == 127991) || (u32 >= 127992 && u32 <= 127994) || (u32 >= 128000 && u32 <= 128007) || (u32 == 128008) || (u32 >= 128009 && u32 <= 128011) || (u32 >= 128012 && u32 <= 128014) || (u32 >= 128015 && u32 <= 128016) || (u32 >= 128017 && u32 <= 128018) || (u32 == 128019) || (u32 == 128020) || (u32 == 128021) || (u32 == 128022) || (u32 >= 128023 && u32 <= 128041) || (u32 == 128042) || (u32 >= 128043 && u32 <= 128062) || (u32 == 128063) || (u32 == 128064) || (u32 == 128065) || (u32 >= 128066 && u32 <= 128100) || (u32 == 128101) || (u32 >= 128102 && u32 <= 128107) || (u32 >= 128108 && u32 <= 128109) || (u32 >= 128110 && u32 <= 128172) || (u32 == 128173) || (u32 >= 128174 && u32 <= 128181) || (u32 >= 128182 && u32 <= 128183) || (u32 >= 128184 && u32 <= 128235) || (u32 >= 128236 && u32 <= 128237) || (u32 == 128238) || (u32 == 128239) || (u32 >= 128240 && u32 <= 128244) || (u32 == 128245) || (u32 >= 128246 && u32 <= 128247) || (u32 == 128248) || (u32 >= 128249 && u32 <= 128252) || (u32 == 128253) || (u32 == 128254) || (u32 >= 128255 && u32 <= 128258) || (u32 == 128259) || (u32 >= 128260 && u32 <= 128263) || (u32 == 128264) || (u32 == 128265) || (u32 >= 128266 && u32 <= 128276) || (u32 == 128277) || (u32 >= 128278 && u32 <= 128299) || (u32 >= 128300 && u32 <= 128301) || (u32 >= 128302 && u32 <= 128317) || (u32 >= 128326 && u32 <= 128328) || (u32 >= 128329 && u32 <= 128330) || (u32 >= 128331 && u32 <= 128334) || (u32 == 128335) || (u32 >= 128336 && u32 <= 128347) || (u32 >= 128348 && u32 <= 128359) || (u32 >= 128360 && u32 <= 128366) || (u32 >= 128367 && u32 <= 128368) || (u32 >= 128369 && u32 <= 128370) || (u32 >= 128371 && u32 <= 128377) || (u32 == 128378) || (u32 >= 128379 && u32 <= 128390) || (u32 == 128391) || (u32 >= 128392 && u32 <= 128393) || (u32 >= 128394 && u32 <= 128397) || (u32 >= 128398 && u32 <= 128399) || (u32 == 128400) || (u32 >= 128401 && u32 <= 128404) || (u32 >= 128405 && u32 <= 128406) || (u32 >= 128407 && u32 <= 128419) || (u32 == 128420) || (u32 == 128421) || (u32 >= 128422 && u32 <= 128423) || (u32 == 128424) || (u32 >= 128425 && u32 <= 128432) || (u32 >= 128433 && u32 <= 128434) || (u32 >= 128435 && u32 <= 128443) || (u32 == 128444) || (u32 >= 128445 && u32 <= 128449) || (u32 >= 128450 && u32 <= 128452) || (u32 >= 128453 && u32 <= 128464) || (u32 >= 128465 && u32 <= 128467) || (u32 >= 128468 && u32 <= 128475) || (u32 >= 128476 && u32 <= 128478) || (u32 >= 128479 && u32 <= 128480) || (u32 == 128481) || (u32 == 128482) || (u32 == 128483) || (u32 >= 128484 && u32 <= 128487) || (u32 == 128488) || (u32 >= 128489 && u32 <= 128494) || (u32 == 128495) || (u32 >= 128496 && u32 <= 128498) || (u32 == 128499) || (u32 >= 128500 && u32 <= 128505) || (u32 == 128506) || (u32 >= 128507 && u32 <= 128511) || (u32 == 128512) || (u32 >= 128513 && u32 <= 128518) || (u32 >= 128519 && u32 <= 128520) || (u32 >= 128521 && u32 <= 128525) || (u32 == 128526) || (u32 == 128527) || (u32 == 128528) || (u32 == 128529) || (u32 >= 128530 && u32 <= 128532) || (u32 == 128533) || (u32 == 128534) || (u32 == 128535) || (u32 == 128536) || (u32 == 128537) || (u32 == 128538) || (u32 == 128539) || (u32 >= 128540 && u32 <= 128542) || (u32 == 128543) || (u32 >= 128544 && u32 <= 128549) || (u32 >= 128550 && u32 <= 128551) || (u32 >= 128552 && u32 <= 128555) || (u32 == 128556) || (u32 == 128557) || (u32 >= 128558 && u32 <= 128559) || (u32 >= 128560 && u32 <= 128563) || (u32 == 128564) || (u32 == 128565) || (u32 == 128566) || (u32 >= 128567 && u32 <= 128576) || (u32 >= 128577 && u32 <= 128580) || (u32 >= 128581 && u32 <= 128591) || (u32 == 128640) || (u32 >= 128641 && u32 <= 128642) || (u32 >= 128643 && u32 <= 128645) || (u32 == 128646) || (u32 == 128647) || (u32 == 128648) || (u32 == 128649) || (u32 >= 128650 && u32 <= 128651) || (u32 == 128652) || (u32 == 128653) || (u32 == 128654) || (u32 == 128655) || (u32 == 128656) || (u32 >= 128657 && u32 <= 128659) || (u32 == 128660) || (u32 == 128661) || (u32 == 128662) || (u32 == 128663) || (u32 == 128664) || (u32 >= 128665 && u32 <= 128666) || (u32 >= 128667 && u32 <= 128673) || (u32 == 128674) || (u32 == 128675) || (u32 >= 128676 && u32 <= 128677) || (u32 == 128678) || (u32 >= 128679 && u32 <= 128685) || (u32 >= 128686 && u32 <= 128689) || (u32 == 128690) || (u32 >= 128691 && u32 <= 128693) || (u32 == 128694) || (u32 >= 128695 && u32 <= 128696) || (u32 >= 128697 && u32 <= 128702) || (u32 == 128703) || (u32 == 128704) || (u32 >= 128705 && u32 <= 128709) || (u32 >= 128710 && u32 <= 128714) || (u32 == 128715) || (u32 == 128716) || (u32 >= 128717 && u32 <= 128719) || (u32 == 128720) || (u32 >= 128721 && u32 <= 128722) || (u32 >= 128723 && u32 <= 128724) || (u32 == 128725) || (u32 >= 128726 && u32 <= 128727) || (u32 >= 128728 && u32 <= 128731) || (u32 == 128732) || (u32 >= 128733 && u32 <= 128735) || (u32 >= 128736 && u32 <= 128741) || (u32 >= 128742 && u32 <= 128744) || (u32 == 128745) || (u32 == 128746) || (u32 >= 128747 && u32 <= 128748) || (u32 >= 128749 && u32 <= 128751) || (u32 == 128752) || (u32 >= 128753 && u32 <= 128754) || (u32 == 128755) || (u32 >= 128756 && u32 <= 128758) || (u32 >= 128759 && u32 <= 128760) || (u32 == 128761) || (u32 == 128762) || (u32 >= 128763 && u32 <= 128764) || (u32 >= 128765 && u32 <= 128767) || (u32 >= 128884 && u32 <= 128895) || (u32 >= 128981 && u32 <= 128991) || (u32 >= 128992 && u32 <= 129003) || (u32 >= 129004 && u32 <= 129007) || (u32 == 129008) || (u32 >= 129009 && u32 <= 129023) || (u32 >= 129036 && u32 <= 129039) || (u32 >= 129096 && u32 <= 129103) || (u32 >= 129114 && u32 <= 129119) || (u32 >= 129160 && u32 <= 129167) || (u32 >= 129198 && u32 <= 129279) || (u32 == 129292) || (u32 >= 129293 && u32 <= 129295) || (u32 >= 129296 && u32 <= 129304) || (u32 >= 129305 && u32 <= 129310) || (u32 == 129311) || (u32 >= 129312 && u32 <= 129319) || (u32 >= 129320 && u32 <= 129327) || (u32 == 129328) || (u32 >= 129329 && u32 <= 129330) || (u32 >= 129331 && u32 <= 129338) || (u32 >= 129340 && u32 <= 129342) || (u32 == 129343) || (u32 >= 129344 && u32 <= 129349) || (u32 >= 129351 && u32 <= 129355) || (u32 == 129356) || (u32 >= 129357 && u32 <= 129359) || (u32 >= 129360 && u32 <= 129374) || (u32 >= 129375 && u32 <= 129387) || (u32 >= 129388 && u32 <= 129392) || (u32 == 129393) || (u32 == 129394) || (u32 >= 129395 && u32 <= 129398) || (u32 >= 129399 && u32 <= 129400) || (u32 == 129401) || (u32 == 129402) || (u32 == 129403) || (u32 >= 129404 && u32 <= 129407) || (u32 >= 129408 && u32 <= 129412) || (u32 >= 129413 && u32 <= 129425) || (u32 >= 129426 && u32 <= 129431) || (u32 >= 129432 && u32 <= 129442) || (u32 >= 129443 && u32 <= 129444) || (u32 >= 129445 && u32 <= 129450) || (u32 >= 129451 && u32 <= 129453) || (u32 >= 129454 && u32 <= 129455) || (u32 >= 129456 && u32 <= 129465) || (u32 >= 129466 && u32 <= 129471) || (u32 == 129472) || (u32 >= 129473 && u32 <= 129474) || (u32 >= 129475 && u32 <= 129482) || (u32 == 129483) || (u32 == 129484) || (u32 >= 129485 && u32 <= 129487) || (u32 >= 129488 && u32 <= 129510) || (u32 >= 129511 && u32 <= 129535) || (u32 >= 129536 && u32 <= 129647) || (u32 >= 129648 && u32 <= 129651) || (u32 == 129652) || (u32 >= 129653 && u32 <= 129655) || (u32 >= 129656 && u32 <= 129658) || (u32 >= 129659 && u32 <= 129660) || (u32 >= 129661 && u32 <= 129663) || (u32 >= 129664 && u32 <= 129666) || (u32 >= 129667 && u32 <= 129670) || (u32 >= 129671 && u32 <= 129672) || (u32 >= 129673 && u32 <= 129679) || (u32 >= 129680 && u32 <= 129685) || (u32 >= 129686 && u32 <= 129704) || (u32 >= 129705 && u32 <= 129708) || (u32 >= 129709 && u32 <= 129711) || (u32 >= 129712 && u32 <= 129718) || (u32 >= 129719 && u32 <= 129722) || (u32 >= 129723 && u32 <= 129725) || (u32 == 129726) || (u32 == 129727) || (u32 >= 129728 && u32 <= 129730) || (u32 >= 129731 && u32 <= 129733) || (u32 >= 129734 && u32 <= 129741) || (u32 >= 129742 && u32 <= 129743) || (u32 >= 129744 && u32 <= 129750) || (u32 >= 129751 && u32 <= 129753) || (u32 >= 129754 && u32 <= 129755) || (u32 >= 129756 && u32 <= 129759) || (u32 >= 129760 && u32 <= 129767) || (u32 == 129768) || (u32 >= 129769 && u32 <= 129775) || (u32 >= 129776 && u32 <= 129782) || (u32 >= 129783 && u32 <= 129784) || (u32 >= 129785 && u32 <= 129791) || (u32 >= 130048 && u32 <= 131069)

isBase : U32 -> Bool
isBase = \u32 -> (u32 == 9757) || (u32 == 9977) || (u32 >= 9994 && u32 <= 9996) || (u32 == 9997) || (u32 == 127877) || (u32 >= 127938 && u32 <= 127940) || (u32 == 127943) || (u32 == 127946) || (u32 >= 127947 && u32 <= 127948) || (u32 >= 128066 && u32 <= 128067) || (u32 >= 128070 && u32 <= 128080) || (u32 >= 128102 && u32 <= 128107) || (u32 >= 128108 && u32 <= 128109) || (u32 >= 128110 && u32 <= 128120) || (u32 == 128124) || (u32 >= 128129 && u32 <= 128131) || (u32 >= 128133 && u32 <= 128135) || (u32 == 128143) || (u32 == 128145) || (u32 == 128170) || (u32 >= 128372 && u32 <= 128373) || (u32 == 128378) || (u32 == 128400) || (u32 >= 128405 && u32 <= 128406) || (u32 >= 128581 && u32 <= 128583) || (u32 >= 128587 && u32 <= 128591) || (u32 == 128675) || (u32 >= 128692 && u32 <= 128693) || (u32 == 128694) || (u32 == 128704) || (u32 == 128716) || (u32 == 129292) || (u32 == 129295) || (u32 == 129304) || (u32 >= 129305 && u32 <= 129310) || (u32 == 129311) || (u32 == 129318) || (u32 == 129328) || (u32 >= 129329 && u32 <= 129330) || (u32 >= 129331 && u32 <= 129337) || (u32 >= 129340 && u32 <= 129342) || (u32 == 129399) || (u32 >= 129461 && u32 <= 129462) || (u32 >= 129464 && u32 <= 129465) || (u32 == 129467) || (u32 >= 129485 && u32 <= 129487) || (u32 >= 129489 && u32 <= 129501) || (u32 >= 129731 && u32 <= 129733) || (u32 >= 129776 && u32 <= 129782) || (u32 >= 129783 && u32 <= 129784)

isModifier : U32 -> Bool
isModifier = \u32 -> (u32 >= 127995 && u32 <= 127999)

isPresentation : U32 -> Bool
isPresentation = \u32 -> (u32 >= 8986 && u32 <= 8987) || (u32 >= 9193 && u32 <= 9196) || (u32 == 9200) || (u32 == 9203) || (u32 >= 9725 && u32 <= 9726) || (u32 >= 9748 && u32 <= 9749) || (u32 >= 9800 && u32 <= 9811) || (u32 == 9855) || (u32 == 9875) || (u32 == 9889) || (u32 >= 9898 && u32 <= 9899) || (u32 >= 9917 && u32 <= 9918) || (u32 >= 9924 && u32 <= 9925) || (u32 == 9934) || (u32 == 9940) || (u32 == 9962) || (u32 >= 9970 && u32 <= 9971) || (u32 == 9973) || (u32 == 9978) || (u32 == 9981) || (u32 == 9989) || (u32 >= 9994 && u32 <= 9995) || (u32 == 10024) || (u32 == 10060) || (u32 == 10062) || (u32 >= 10067 && u32 <= 10069) || (u32 == 10071) || (u32 >= 10133 && u32 <= 10135) || (u32 == 10160) || (u32 == 10175) || (u32 >= 11035 && u32 <= 11036) || (u32 == 11088) || (u32 == 11093) || (u32 == 126980) || (u32 == 127183) || (u32 == 127374) || (u32 >= 127377 && u32 <= 127386) || (u32 >= 127462 && u32 <= 127487) || (u32 == 127489) || (u32 == 127514) || (u32 == 127535) || (u32 >= 127538 && u32 <= 127542) || (u32 >= 127544 && u32 <= 127546) || (u32 >= 127568 && u32 <= 127569) || (u32 >= 127744 && u32 <= 127756) || (u32 >= 127757 && u32 <= 127758) || (u32 == 127759) || (u32 == 127760) || (u32 == 127761) || (u32 == 127762) || (u32 >= 127763 && u32 <= 127765) || (u32 >= 127766 && u32 <= 127768) || (u32 == 127769) || (u32 == 127770) || (u32 == 127771) || (u32 == 127772) || (u32 >= 127773 && u32 <= 127774) || (u32 >= 127775 && u32 <= 127776) || (u32 >= 127789 && u32 <= 127791) || (u32 >= 127792 && u32 <= 127793) || (u32 >= 127794 && u32 <= 127795) || (u32 >= 127796 && u32 <= 127797) || (u32 >= 127799 && u32 <= 127818) || (u32 == 127819) || (u32 >= 127820 && u32 <= 127823) || (u32 == 127824) || (u32 >= 127825 && u32 <= 127867) || (u32 == 127868) || (u32 >= 127870 && u32 <= 127871) || (u32 >= 127872 && u32 <= 127891) || (u32 >= 127904 && u32 <= 127940) || (u32 == 127941) || (u32 == 127942) || (u32 == 127943) || (u32 == 127944) || (u32 == 127945) || (u32 == 127946) || (u32 >= 127951 && u32 <= 127955) || (u32 >= 127968 && u32 <= 127971) || (u32 == 127972) || (u32 >= 127973 && u32 <= 127984) || (u32 == 127988) || (u32 >= 127992 && u32 <= 128007) || (u32 == 128008) || (u32 >= 128009 && u32 <= 128011) || (u32 >= 128012 && u32 <= 128014) || (u32 >= 128015 && u32 <= 128016) || (u32 >= 128017 && u32 <= 128018) || (u32 == 128019) || (u32 == 128020) || (u32 == 128021) || (u32 == 128022) || (u32 >= 128023 && u32 <= 128041) || (u32 == 128042) || (u32 >= 128043 && u32 <= 128062) || (u32 == 128064) || (u32 >= 128066 && u32 <= 128100) || (u32 == 128101) || (u32 >= 128102 && u32 <= 128107) || (u32 >= 128108 && u32 <= 128109) || (u32 >= 128110 && u32 <= 128172) || (u32 == 128173) || (u32 >= 128174 && u32 <= 128181) || (u32 >= 128182 && u32 <= 128183) || (u32 >= 128184 && u32 <= 128235) || (u32 >= 128236 && u32 <= 128237) || (u32 == 128238) || (u32 == 128239) || (u32 >= 128240 && u32 <= 128244) || (u32 == 128245) || (u32 >= 128246 && u32 <= 128247) || (u32 == 128248) || (u32 >= 128249 && u32 <= 128252) || (u32 >= 128255 && u32 <= 128258) || (u32 == 128259) || (u32 >= 128260 && u32 <= 128263) || (u32 == 128264) || (u32 == 128265) || (u32 >= 128266 && u32 <= 128276) || (u32 == 128277) || (u32 >= 128278 && u32 <= 128299) || (u32 >= 128300 && u32 <= 128301) || (u32 >= 128302 && u32 <= 128317) || (u32 >= 128331 && u32 <= 128334) || (u32 >= 128336 && u32 <= 128347) || (u32 >= 128348 && u32 <= 128359) || (u32 == 128378) || (u32 >= 128405 && u32 <= 128406) || (u32 == 128420) || (u32 >= 128507 && u32 <= 128511) || (u32 == 128512) || (u32 >= 128513 && u32 <= 128518) || (u32 >= 128519 && u32 <= 128520) || (u32 >= 128521 && u32 <= 128525) || (u32 == 128526) || (u32 == 128527) || (u32 == 128528) || (u32 == 128529) || (u32 >= 128530 && u32 <= 128532) || (u32 == 128533) || (u32 == 128534) || (u32 == 128535) || (u32 == 128536) || (u32 == 128537) || (u32 == 128538) || (u32 == 128539) || (u32 >= 128540 && u32 <= 128542) || (u32 == 128543) || (u32 >= 128544 && u32 <= 128549) || (u32 >= 128550 && u32 <= 128551) || (u32 >= 128552 && u32 <= 128555) || (u32 == 128556) || (u32 == 128557) || (u32 >= 128558 && u32 <= 128559) || (u32 >= 128560 && u32 <= 128563) || (u32 == 128564) || (u32 == 128565) || (u32 == 128566) || (u32 >= 128567 && u32 <= 128576) || (u32 >= 128577 && u32 <= 128580) || (u32 >= 128581 && u32 <= 128591) || (u32 == 128640) || (u32 >= 128641 && u32 <= 128642) || (u32 >= 128643 && u32 <= 128645) || (u32 == 128646) || (u32 == 128647) || (u32 == 128648) || (u32 == 128649) || (u32 >= 128650 && u32 <= 128651) || (u32 == 128652) || (u32 == 128653) || (u32 == 128654) || (u32 == 128655) || (u32 == 128656) || (u32 >= 128657 && u32 <= 128659) || (u32 == 128660) || (u32 == 128661) || (u32 == 128662) || (u32 == 128663) || (u32 == 128664) || (u32 >= 128665 && u32 <= 128666) || (u32 >= 128667 && u32 <= 128673) || (u32 == 128674) || (u32 == 128675) || (u32 >= 128676 && u32 <= 128677) || (u32 == 128678) || (u32 >= 128679 && u32 <= 128685) || (u32 >= 128686 && u32 <= 128689) || (u32 == 128690) || (u32 >= 128691 && u32 <= 128693) || (u32 == 128694) || (u32 >= 128695 && u32 <= 128696) || (u32 >= 128697 && u32 <= 128702) || (u32 == 128703) || (u32 == 128704) || (u32 >= 128705 && u32 <= 128709) || (u32 == 128716) || (u32 == 128720) || (u32 >= 128721 && u32 <= 128722) || (u32 == 128725) || (u32 >= 128726 && u32 <= 128727) || (u32 == 128732) || (u32 >= 128733 && u32 <= 128735) || (u32 >= 128747 && u32 <= 128748) || (u32 >= 128756 && u32 <= 128758) || (u32 >= 128759 && u32 <= 128760) || (u32 == 128761) || (u32 == 128762) || (u32 >= 128763 && u32 <= 128764) || (u32 >= 128992 && u32 <= 129003) || (u32 == 129008) || (u32 == 129292) || (u32 >= 129293 && u32 <= 129295) || (u32 >= 129296 && u32 <= 129304) || (u32 >= 129305 && u32 <= 129310) || (u32 == 129311) || (u32 >= 129312 && u32 <= 129319) || (u32 >= 129320 && u32 <= 129327) || (u32 == 129328) || (u32 >= 129329 && u32 <= 129330) || (u32 >= 129331 && u32 <= 129338) || (u32 >= 129340 && u32 <= 129342) || (u32 == 129343) || (u32 >= 129344 && u32 <= 129349) || (u32 >= 129351 && u32 <= 129355) || (u32 == 129356) || (u32 >= 129357 && u32 <= 129359) || (u32 >= 129360 && u32 <= 129374) || (u32 >= 129375 && u32 <= 129387) || (u32 >= 129388 && u32 <= 129392) || (u32 == 129393) || (u32 == 129394) || (u32 >= 129395 && u32 <= 129398) || (u32 >= 129399 && u32 <= 129400) || (u32 == 129401) || (u32 == 129402) || (u32 == 129403) || (u32 >= 129404 && u32 <= 129407) || (u32 >= 129408 && u32 <= 129412) || (u32 >= 129413 && u32 <= 129425) || (u32 >= 129426 && u32 <= 129431) || (u32 >= 129432 && u32 <= 129442) || (u32 >= 129443 && u32 <= 129444) || (u32 >= 129445 && u32 <= 129450) || (u32 >= 129451 && u32 <= 129453) || (u32 >= 129454 && u32 <= 129455) || (u32 >= 129456 && u32 <= 129465) || (u32 >= 129466 && u32 <= 129471) || (u32 == 129472) || (u32 >= 129473 && u32 <= 129474) || (u32 >= 129475 && u32 <= 129482) || (u32 == 129483) || (u32 == 129484) || (u32 >= 129485 && u32 <= 129487) || (u32 >= 129488 && u32 <= 129510) || (u32 >= 129511 && u32 <= 129535) || (u32 >= 129648 && u32 <= 129651) || (u32 == 129652) || (u32 >= 129653 && u32 <= 129655) || (u32 >= 129656 && u32 <= 129658) || (u32 >= 129659 && u32 <= 129660) || (u32 >= 129664 && u32 <= 129666) || (u32 >= 129667 && u32 <= 129670) || (u32 >= 129671 && u32 <= 129672) || (u32 >= 129680 && u32 <= 129685) || (u32 >= 129686 && u32 <= 129704) || (u32 >= 129705 && u32 <= 129708) || (u32 >= 129709 && u32 <= 129711) || (u32 >= 129712 && u32 <= 129718) || (u32 >= 129719 && u32 <= 129722) || (u32 >= 129723 && u32 <= 129725) || (u32 == 129727) || (u32 >= 129728 && u32 <= 129730) || (u32 >= 129731 && u32 <= 129733) || (u32 >= 129742 && u32 <= 129743) || (u32 >= 129744 && u32 <= 129750) || (u32 >= 129751 && u32 <= 129753) || (u32 >= 129754 && u32 <= 129755) || (u32 >= 129760 && u32 <= 129767) || (u32 == 129768) || (u32 >= 129776 && u32 <= 129782) || (u32 >= 129783 && u32 <= 129784)

isComponent : U32 -> Bool
isComponent = \u32 -> (u32 == 35) || (u32 == 42) || (u32 >= 48 && u32 <= 57) || (u32 == 8205) || (u32 == 8419) || (u32 == 65039) || (u32 >= 127462 && u32 <= 127487) || (u32 >= 127995 && u32 <= 127999) || (u32 >= 129456 && u32 <= 129459) || (u32 >= 917536 && u32 <= 917631)

isEmoji : U32 -> Bool
isEmoji = \u32 -> (u32 == 35) || (u32 == 42) || (u32 >= 48 && u32 <= 57) || (u32 == 169) || (u32 == 174) || (u32 == 8252) || (u32 == 8265) || (u32 == 8482) || (u32 == 8505) || (u32 >= 8596 && u32 <= 8601) || (u32 >= 8617 && u32 <= 8618) || (u32 >= 8986 && u32 <= 8987) || (u32 == 9000) || (u32 == 9167) || (u32 >= 9193 && u32 <= 9196) || (u32 >= 9197 && u32 <= 9198) || (u32 == 9199) || (u32 == 9200) || (u32 >= 9201 && u32 <= 9202) || (u32 == 9203) || (u32 >= 9208 && u32 <= 9210) || (u32 == 9410) || (u32 >= 9642 && u32 <= 9643) || (u32 == 9654) || (u32 == 9664) || (u32 >= 9723 && u32 <= 9726) || (u32 >= 9728 && u32 <= 9729) || (u32 >= 9730 && u32 <= 9731) || (u32 == 9732) || (u32 == 9742) || (u32 == 9745) || (u32 >= 9748 && u32 <= 9749) || (u32 == 9752) || (u32 == 9757) || (u32 == 9760) || (u32 >= 9762 && u32 <= 9763) || (u32 == 9766) || (u32 == 9770) || (u32 == 9774) || (u32 == 9775) || (u32 >= 9784 && u32 <= 9785) || (u32 == 9786) || (u32 == 9792) || (u32 == 9794) || (u32 >= 9800 && u32 <= 9811) || (u32 == 9823) || (u32 == 9824) || (u32 == 9827) || (u32 >= 9829 && u32 <= 9830) || (u32 == 9832) || (u32 == 9851) || (u32 == 9854) || (u32 == 9855) || (u32 == 9874) || (u32 == 9875) || (u32 == 9876) || (u32 == 9877) || (u32 >= 9878 && u32 <= 9879) || (u32 == 9881) || (u32 >= 9883 && u32 <= 9884) || (u32 >= 9888 && u32 <= 9889) || (u32 == 9895) || (u32 >= 9898 && u32 <= 9899) || (u32 >= 9904 && u32 <= 9905) || (u32 >= 9917 && u32 <= 9918) || (u32 >= 9924 && u32 <= 9925) || (u32 == 9928) || (u32 == 9934) || (u32 == 9935) || (u32 == 9937) || (u32 == 9939) || (u32 == 9940) || (u32 == 9961) || (u32 == 9962) || (u32 >= 9968 && u32 <= 9969) || (u32 >= 9970 && u32 <= 9971) || (u32 == 9972) || (u32 == 9973) || (u32 >= 9975 && u32 <= 9977) || (u32 == 9978) || (u32 == 9981) || (u32 == 9986) || (u32 == 9989) || (u32 >= 9992 && u32 <= 9996) || (u32 == 9997) || (u32 == 9999) || (u32 == 10002) || (u32 == 10004) || (u32 == 10006) || (u32 == 10013) || (u32 == 10017) || (u32 == 10024) || (u32 >= 10035 && u32 <= 10036) || (u32 == 10052) || (u32 == 10055) || (u32 == 10060) || (u32 == 10062) || (u32 >= 10067 && u32 <= 10069) || (u32 == 10071) || (u32 == 10083) || (u32 == 10084) || (u32 >= 10133 && u32 <= 10135) || (u32 == 10145) || (u32 == 10160) || (u32 == 10175) || (u32 >= 10548 && u32 <= 10549) || (u32 >= 11013 && u32 <= 11015) || (u32 >= 11035 && u32 <= 11036) || (u32 == 11088) || (u32 == 11093) || (u32 == 12336) || (u32 == 12349) || (u32 == 12951) || (u32 == 12953) || (u32 == 126980) || (u32 == 127183) || (u32 >= 127344 && u32 <= 127345) || (u32 >= 127358 && u32 <= 127359) || (u32 == 127374) || (u32 >= 127377 && u32 <= 127386) || (u32 >= 127462 && u32 <= 127487) || (u32 >= 127489 && u32 <= 127490) || (u32 == 127514) || (u32 == 127535) || (u32 >= 127538 && u32 <= 127546) || (u32 >= 127568 && u32 <= 127569) || (u32 >= 127744 && u32 <= 127756) || (u32 >= 127757 && u32 <= 127758) || (u32 == 127759) || (u32 == 127760) || (u32 == 127761) || (u32 == 127762) || (u32 >= 127763 && u32 <= 127765) || (u32 >= 127766 && u32 <= 127768) || (u32 == 127769) || (u32 == 127770) || (u32 == 127771) || (u32 == 127772) || (u32 >= 127773 && u32 <= 127774) || (u32 >= 127775 && u32 <= 127776) || (u32 == 127777) || (u32 >= 127780 && u32 <= 127788) || (u32 >= 127789 && u32 <= 127791) || (u32 >= 127792 && u32 <= 127793) || (u32 >= 127794 && u32 <= 127795) || (u32 >= 127796 && u32 <= 127797) || (u32 == 127798) || (u32 >= 127799 && u32 <= 127818) || (u32 == 127819) || (u32 >= 127820 && u32 <= 127823) || (u32 == 127824) || (u32 >= 127825 && u32 <= 127867) || (u32 == 127868) || (u32 == 127869) || (u32 >= 127870 && u32 <= 127871) || (u32 >= 127872 && u32 <= 127891) || (u32 >= 127894 && u32 <= 127895) || (u32 >= 127897 && u32 <= 127899) || (u32 >= 127902 && u32 <= 127903) || (u32 >= 127904 && u32 <= 127940) || (u32 == 127941) || (u32 == 127942) || (u32 == 127943) || (u32 == 127944) || (u32 == 127945) || (u32 == 127946) || (u32 >= 127947 && u32 <= 127950) || (u32 >= 127951 && u32 <= 127955) || (u32 >= 127956 && u32 <= 127967) || (u32 >= 127968 && u32 <= 127971) || (u32 == 127972) || (u32 >= 127973 && u32 <= 127984) || (u32 == 127987) || (u32 == 127988) || (u32 == 127989) || (u32 == 127991) || (u32 >= 127992 && u32 <= 128007) || (u32 == 128008) || (u32 >= 128009 && u32 <= 128011) || (u32 >= 128012 && u32 <= 128014) || (u32 >= 128015 && u32 <= 128016) || (u32 >= 128017 && u32 <= 128018) || (u32 == 128019) || (u32 == 128020) || (u32 == 128021) || (u32 == 128022) || (u32 >= 128023 && u32 <= 128041) || (u32 == 128042) || (u32 >= 128043 && u32 <= 128062) || (u32 == 128063) || (u32 == 128064) || (u32 == 128065) || (u32 >= 128066 && u32 <= 128100) || (u32 == 128101) || (u32 >= 128102 && u32 <= 128107) || (u32 >= 128108 && u32 <= 128109) || (u32 >= 128110 && u32 <= 128172) || (u32 == 128173) || (u32 >= 128174 && u32 <= 128181) || (u32 >= 128182 && u32 <= 128183) || (u32 >= 128184 && u32 <= 128235) || (u32 >= 128236 && u32 <= 128237) || (u32 == 128238) || (u32 == 128239) || (u32 >= 128240 && u32 <= 128244) || (u32 == 128245) || (u32 >= 128246 && u32 <= 128247) || (u32 == 128248) || (u32 >= 128249 && u32 <= 128252) || (u32 == 128253) || (u32 >= 128255 && u32 <= 128258) || (u32 == 128259) || (u32 >= 128260 && u32 <= 128263) || (u32 == 128264) || (u32 == 128265) || (u32 >= 128266 && u32 <= 128276) || (u32 == 128277) || (u32 >= 128278 && u32 <= 128299) || (u32 >= 128300 && u32 <= 128301) || (u32 >= 128302 && u32 <= 128317) || (u32 >= 128329 && u32 <= 128330) || (u32 >= 128331 && u32 <= 128334) || (u32 >= 128336 && u32 <= 128347) || (u32 >= 128348 && u32 <= 128359) || (u32 >= 128367 && u32 <= 128368) || (u32 >= 128371 && u32 <= 128377) || (u32 == 128378) || (u32 == 128391) || (u32 >= 128394 && u32 <= 128397) || (u32 == 128400) || (u32 >= 128405 && u32 <= 128406) || (u32 == 128420) || (u32 == 128421) || (u32 == 128424) || (u32 >= 128433 && u32 <= 128434) || (u32 == 128444) || (u32 >= 128450 && u32 <= 128452) || (u32 >= 128465 && u32 <= 128467) || (u32 >= 128476 && u32 <= 128478) || (u32 == 128481) || (u32 == 128483) || (u32 == 128488) || (u32 == 128495) || (u32 == 128499) || (u32 == 128506) || (u32 >= 128507 && u32 <= 128511) || (u32 == 128512) || (u32 >= 128513 && u32 <= 128518) || (u32 >= 128519 && u32 <= 128520) || (u32 >= 128521 && u32 <= 128525) || (u32 == 128526) || (u32 == 128527) || (u32 == 128528) || (u32 == 128529) || (u32 >= 128530 && u32 <= 128532) || (u32 == 128533) || (u32 == 128534) || (u32 == 128535) || (u32 == 128536) || (u32 == 128537) || (u32 == 128538) || (u32 == 128539) || (u32 >= 128540 && u32 <= 128542) || (u32 == 128543) || (u32 >= 128544 && u32 <= 128549) || (u32 >= 128550 && u32 <= 128551) || (u32 >= 128552 && u32 <= 128555) || (u32 == 128556) || (u32 == 128557) || (u32 >= 128558 && u32 <= 128559) || (u32 >= 128560 && u32 <= 128563) || (u32 == 128564) || (u32 == 128565) || (u32 == 128566) || (u32 >= 128567 && u32 <= 128576) || (u32 >= 128577 && u32 <= 128580) || (u32 >= 128581 && u32 <= 128591) || (u32 == 128640) || (u32 >= 128641 && u32 <= 128642) || (u32 >= 128643 && u32 <= 128645) || (u32 == 128646) || (u32 == 128647) || (u32 == 128648) || (u32 == 128649) || (u32 >= 128650 && u32 <= 128651) || (u32 == 128652) || (u32 == 128653) || (u32 == 128654) || (u32 == 128655) || (u32 == 128656) || (u32 >= 128657 && u32 <= 128659) || (u32 == 128660) || (u32 == 128661) || (u32 == 128662) || (u32 == 128663) || (u32 == 128664) || (u32 >= 128665 && u32 <= 128666) || (u32 >= 128667 && u32 <= 128673) || (u32 == 128674) || (u32 == 128675) || (u32 >= 128676 && u32 <= 128677) || (u32 == 128678) || (u32 >= 128679 && u32 <= 128685) || (u32 >= 128686 && u32 <= 128689) || (u32 == 128690) || (u32 >= 128691 && u32 <= 128693) || (u32 == 128694) || (u32 >= 128695 && u32 <= 128696) || (u32 >= 128697 && u32 <= 128702) || (u32 == 128703) || (u32 == 128704) || (u32 >= 128705 && u32 <= 128709) || (u32 == 128715) || (u32 == 128716) || (u32 >= 128717 && u32 <= 128719) || (u32 == 128720) || (u32 >= 128721 && u32 <= 128722) || (u32 == 128725) || (u32 >= 128726 && u32 <= 128727) || (u32 == 128732) || (u32 >= 128733 && u32 <= 128735) || (u32 >= 128736 && u32 <= 128741) || (u32 == 128745) || (u32 >= 128747 && u32 <= 128748) || (u32 == 128752) || (u32 == 128755) || (u32 >= 128756 && u32 <= 128758) || (u32 >= 128759 && u32 <= 128760) || (u32 == 128761) || (u32 == 128762) || (u32 >= 128763 && u32 <= 128764) || (u32 >= 128992 && u32 <= 129003) || (u32 == 129008) || (u32 == 129292) || (u32 >= 129293 && u32 <= 129295) || (u32 >= 129296 && u32 <= 129304) || (u32 >= 129305 && u32 <= 129310) || (u32 == 129311) || (u32 >= 129312 && u32 <= 129319) || (u32 >= 129320 && u32 <= 129327) || (u32 == 129328) || (u32 >= 129329 && u32 <= 129330) || (u32 >= 129331 && u32 <= 129338) || (u32 >= 129340 && u32 <= 129342) || (u32 == 129343) || (u32 >= 129344 && u32 <= 129349) || (u32 >= 129351 && u32 <= 129355) || (u32 == 129356) || (u32 >= 129357 && u32 <= 129359) || (u32 >= 129360 && u32 <= 129374) || (u32 >= 129375 && u32 <= 129387) || (u32 >= 129388 && u32 <= 129392) || (u32 == 129393) || (u32 == 129394) || (u32 >= 129395 && u32 <= 129398) || (u32 >= 129399 && u32 <= 129400) || (u32 == 129401) || (u32 == 129402) || (u32 == 129403) || (u32 >= 129404 && u32 <= 129407) || (u32 >= 129408 && u32 <= 129412) || (u32 >= 129413 && u32 <= 129425) || (u32 >= 129426 && u32 <= 129431) || (u32 >= 129432 && u32 <= 129442) || (u32 >= 129443 && u32 <= 129444) || (u32 >= 129445 && u32 <= 129450) || (u32 >= 129451 && u32 <= 129453) || (u32 >= 129454 && u32 <= 129455) || (u32 >= 129456 && u32 <= 129465) || (u32 >= 129466 && u32 <= 129471) || (u32 == 129472) || (u32 >= 129473 && u32 <= 129474) || (u32 >= 129475 && u32 <= 129482) || (u32 == 129483) || (u32 == 129484) || (u32 >= 129485 && u32 <= 129487) || (u32 >= 129488 && u32 <= 129510) || (u32 >= 129511 && u32 <= 129535) || (u32 >= 129648 && u32 <= 129651) || (u32 == 129652) || (u32 >= 129653 && u32 <= 129655) || (u32 >= 129656 && u32 <= 129658) || (u32 >= 129659 && u32 <= 129660) || (u32 >= 129664 && u32 <= 129666) || (u32 >= 129667 && u32 <= 129670) || (u32 >= 129671 && u32 <= 129672) || (u32 >= 129680 && u32 <= 129685) || (u32 >= 129686 && u32 <= 129704) || (u32 >= 129705 && u32 <= 129708) || (u32 >= 129709 && u32 <= 129711) || (u32 >= 129712 && u32 <= 129718) || (u32 >= 129719 && u32 <= 129722) || (u32 >= 129723 && u32 <= 129725) || (u32 == 129727) || (u32 >= 129728 && u32 <= 129730) || (u32 >= 129731 && u32 <= 129733) || (u32 >= 129742 && u32 <= 129743) || (u32 >= 129744 && u32 <= 129750) || (u32 >= 129751 && u32 <= 129753) || (u32 >= 129754 && u32 <= 129755) || (u32 >= 129760 && u32 <= 129767) || (u32 == 129768) || (u32 >= 129776 && u32 <= 129782) || (u32 >= 129783 && u32 <= 129784)

fromCP : CP -> Result EMOJI [NonEmojiCodePoint]
fromCP = \cp -> 
    
    u32 = toU32 cp

    if isPictographic u32 then
        Ok Pictographic
    else if isBase u32 then
        Ok Base
    else if isModifier u32 then
        Ok Modifier
    else if isPresentation u32 then
        Ok Presentation
    else if isComponent u32 then
        Ok Component
    else if isEmoji u32 then
        Ok Emoji
    else 
        Err NonEmojiCodePoint