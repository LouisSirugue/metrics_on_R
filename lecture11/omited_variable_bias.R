* Consider for instance the effect of the number of hours of work per week on annual earnings:
  
  $$\text{Earnings}_i = \hat{\alpha} + \hat{\beta}\text{Hours}_i + \hat{\varepsilon_i}$$
    
    --
    
    ```{r}
  summary(lm(Earnings ~ Hours, asec_2020))$coefficients
  ```
  
  --
    
    <p style = "margin-bottom:1.25cm;"></p>
    
    * Taking at face value, the expected returns to working an additional hour per week amount to $2077.79 in annual earning
  * What do you think about this estimation? 
    * Is it plausible that $\hat{\beta}$ actually captures the effect of hours?
    * What could bias our estimation?
    
    ---
    
    ### 3. Interactions
    
    #### The relationship could be inflated by the fact that males tend to be better paid and to work less often part-time
    
    --
    
    * The sex variable acts here as a confounding factor
  * We need to put it as a control variable
  
  <p style = "margin-bottom:1.25cm;"></p>
    
    --
    
    $$\text{Earnings}_i = \hat{\alpha} + \hat{\beta_1}\text{Hours}_i  + \hat{\beta_2}1\{\text{Sex}_i = \text{Male}\} + \hat{\varepsilon_i}$$
    
    --
    
    <p style = "margin-bottom:1.25cm;"></p>
    
    ```{r}
  summary(lm(Earnings ~ Hours + Sex, asec_2020))$coefficients
  ```
  
  ---
    
    ### 3. Omitted variables
    
    #### Why are the coefficients lower than in the univariate case?
    
    <p style = "margin-bottom:1.25cm;"></p>
    
    --
    
    <ul>
    <li>The coefficient of Hours drops from 2077.79 in the univariate regression to 1953.83 when including sex in the regression</li>
    <ul>
    <li>Part of the estimated effect of hours on earnings was due to the fact that those who work more hours per week tend to be men</li>
    </ul>
    </ul>
    
    <p style = "margin-bottom:1.25cm;"></p>
    
    --
    
    <ul>
    <li>The coefficient of Sex drops from 21612.33 in the univariate regression to 13794.39 when including sex in the regression</li>
    <ul>
    <li>Part of the estimated effect of sex on earnings was due to the fact that men tend to work more hours per week</li>
    </ul>
    </ul>
    
    <p style = "margin-bottom:1.25cm;"></p>
    
    --
    
    <ul>
    <li>There are plenty of omitted factors that could influence our estimates</li>
    <ul>
    <li>How would the coefficients change if we were to control for having an executive position?</li>
    </ul>
    </ul>
    
    ---
    
    ### 3. Interactions
    
    #### &#10140; This is why we cannot talk about causality!
    
    * When 
  