% ANIMAL CLASSIFICATION EXERCISE
%Animals file contains all 13 properties of 16 animals
%Initilialising weights and Map properties
t=1;
d = zeros(10,10);
pattern_size = 13;
map = cell(10,10);
iterations= 10000;
%randomizing map weights
for i = (1:10)
    for j = (1:10)
        map{i,j} = rand(13,1);
    end
end
%randomly pick one of the animals
for t =(1:iterations)
    %r will be used to set a random input of x for each training iteration
    r = round (1 + (16-1)*rand(1,1));
    %format random input of pattern size to train the SLP
    x = Animals(:,r);
    X = x';
    for i = (1:10)
        for j = (1:10)
            w =map{i,j};
            W= w';
            %cycle through the distances of inputs from different weights neuron
            d(i,j)=sqrt((X-W)*(x-w));
        end
    end
    distance =d(1,1);

    for i = (1:10)
        for j = (1:10)
            %try to see if minimum distance
            if d(i,j)<distance;
                min = d(i,j);
                u = i;
                v = j;
            end
        end
    end
    %setting learning parameters
    theta = 1;
    alpha = 100/(200+t);
    for i = (1:10)
        for j = (1:10)
            %Calculate eta for each winning neuron
            eta = exp(-(((i-u).^2)+(j-v).^2)/2*theta.^2) ;
            %update weight
            map{i,j} = map{i,j} + alpha*eta*(x-map{i,j});
        end
    end
end


%TESTING PHASE
for test = (1:16)
    y = Animals(:,test) ;
    Y=y' ;
    for i = (1:10)
        for j = (1:10)
            w=map{i,j};
            W = w';
            %Find the winning neuron based on smallest difference
            d(i,j)=sqrt((Y-W)*(y-w));
        end
    end
    distance = d(1,1);
    for i = (1:10)
        for j = (1:10)
            %getting positions
            if d(i,j)<distance;
                min = d(i,j) ;
                positionx(test) = i ;
                positiony(test) = j ;
            end
        end
    end
end

%Mapping each animal
SOM =[positionx ; positiony]