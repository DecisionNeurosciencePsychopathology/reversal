function  [gx] = g_rev_softmax1(x,phi,u,inG )
% INPUT
% - x : Q-values (2x1)
% - P : inverse temperature (1x1)
% - u : [useless]
% - in : [useless]
% OUTPUT
% - gx : P(a=1|x)


if inG.wsls
    r = u(2);
    choice = u(1);
    if isnan(choice) || r == 0
        p_choice = 0.5;
    else
        if (r == 1 && choice == 1) || (r== -1 && choice == 0) 
            p_choice = 1;
        elseif (r == 1 && choice == 0) || (r== -1 && choice == 1) 
            p_choice = 0;
        end
    end
    
else
    
    beta = exp(phi(1));
    %kappa = phi(2);
    
    dQ = x(1);
    % dQ = (x(1)-x(2));
    % gx = sig(kappa + beta*dQ);
    p_choice = sig(beta*dQ);
    
end
gx = p_choice;

