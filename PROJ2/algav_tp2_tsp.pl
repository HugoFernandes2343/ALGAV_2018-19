
% -----------------------------------------------------------------------
% Trabalho prático: factos de cidades com localização baseada em
% latitude e longitude e predicado auxiliar para calcular a distância
% entre quaisquer duas destas cidades.
% ------------------------------------------------------------------------

%city(name,latitude,longitude)
city(brussels,50.8462807,4.3547273).
city(tirana,41.33165,19.8318).
city(andorra,42.5075025,1.5218033).
city(vienna,48.2092062,16.3727778).
city(minsk,53.905117,27.5611845).
city(sarajevo,43.85643,18.41342).
city(sofia,42.6976246,23.3222924).
city(zagreb,45.8150053,15.9785014).
city(nicosia,35.167604,33.373621).
city(prague,50.0878114,14.4204598).
city(copenhagen,55.6762944,12.5681157).
city(london,51.5001524,-0.1262362).
city(tallinn,59.4388619,24.7544715).
city(helsinki,60.1698791,24.9384078).
city(paris,48.8566667,2.3509871).
city(marseille,43.296386,5.369954).
city(tbilisi,41.709981,44.792998).
city(berlin,52.5234051,13.4113999).
city(athens,37.97918,23.716647).
city(budapest,47.4984056,19.0407578).
city(reykjavik,64.135338,-21.89521).
city(dublin,53.344104,-6.2674937).
city(rome,41.8954656,12.4823243).
city(pristina,42.672421,21.164539).
city(riga,56.9465346,24.1048525).
city(vaduz,47.1410409,9.5214458).
city(vilnius,54.6893865,25.2800243).
city(luxembourg,49.815273,6.129583).
city(skopje,42.003812,21.452246).
city(valletta,35.904171,14.518907).
city(chisinau,47.026859,28.841551).
city(monaco,43.750298,7.412841).
city(podgorica,42.442575,19.268646).
city(amsterdam,52.3738007,4.8909347).
city(belfast,54.5972686,-5.9301088).
city(oslo,59.9138204,10.7387413).
city(warsaw,52.2296756,21.0122287).
city(lisbon,38.7071631,-9.135517).
city(bucharest,44.430481,26.12298).
city(moscow,55.755786,37.617633).
city(san_marino,43.94236,12.457777).
city(edinburgh,55.9501755,-3.1875359).
city(belgrade,44.802416,20.465601).
city(bratislava,48.1483765,17.1073105).
city(ljubljana,46.0514263,14.5059655).
city(madrid,40.4166909,-3.7003454).
city(stockholm,59.3327881,18.0644881).
city(bern,46.9479986,7.4481481).
city(kiev,50.440951,30.5271814).
city(cardiff,51.4813069,-3.1804979).

%  dist_cities(brussels,prague,D).
%  D = 716837.
dist_cities(C1,C2,Dist):-
    city(C1,Lat1,Lon1),
    city(C2,Lat2,Lon2),
    distance(Lat1,Lon1,Lat2,Lon2,Dist).

degrees2radians(Deg,Rad):-
	Rad is Deg*0.0174532925.

% distance(latitude_first_point,longitude_first_point,latitude_second_point,longitude_second_point,distance
% in meters)
distance(Lat1, Lon1, Lat2, Lon2, Dis2):-
	degrees2radians(Lat1,Psi1),
	degrees2radians(Lat2,Psi2),
	DifLat is Lat2-Lat1,
	DifLon is Lon2-Lon1,
	degrees2radians(DifLat,DeltaPsi),
	degrees2radians(DifLon,DeltaLambda),
	A is sin(DeltaPsi/2)*sin(DeltaPsi/2)+ cos(Psi1)*cos(Psi2)*sin(DeltaLambda/2)*sin(DeltaLambda/2),
	C is 2*atan2(sqrt(A),sqrt(1-A)),
	Dis1 is 6371000*C,
	Dis2 is round(Dis1).

% distance(50.8462807,4.3547273,50.0878114,14.4204598,D).
% Online: http://www.movable-type.co.uk/scripts/latlong.html
%



















