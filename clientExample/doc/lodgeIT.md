# Examples from LodgeIT

1. [CGT Assets and Exemptions](../../kb/1_cgt_assets_and_exemptions.pl): A Capital Gain Tax, CGT, asset is (a) any kind of property; or (b) a legal or equitable right that is not property. This document is a first approximation to a computational model of related concepts. It is based on the [List of CGT assets and exemptions](https://www.ato.gov.au/Individuals/Capital-gains-tax/List-of-cgt-assets-and-exemptions/) from the Australian Tax Office. This document can be used to answer questions such as:

    1.1. [which thing is a CGT asset](https://github.com/LogicalContracts/LogicalEnglish/blob/b36ce6e39ccc6ca29ab62397f7f1a5ab7799b8f7/kb/1_cgt_assets_and_exemptions.pl#L506).

    1.2. [which asset is a CGT exempt asset](https://github.com/LogicalContracts/LogicalEnglish/blob/b36ce6e39ccc6ca29ab62397f7f1a5ab7799b8f7/kb/1_cgt_assets_and_exemptions.pl#L509). 

    1.3. [which taxpayer pays CGT for which asset on which date](https://github.com/LogicalContracts/LogicalEnglish/blob/b36ce6e39ccc6ca29ab62397f7f1a5ab7799b8f7/kb/1_cgt_assets_and_exemptions.pl#L515).

2. [Meaning of net value of the CGT assets](../../kb/1_net_asset_value_test_2.pl) is a LE document inspired by the guideline [Maximum net asset value test](https://www.ato.gov.au/Business/Small-business-entity-concessions/Concessions/CGT-concessions/Maximum-net-asset-value-test/) of the Australian Tax Office, but actually based on the original source of that legislation: [Income Tax Assessment Act 1997](https://www.ato.gov.au/law/view/document?docid=PAC/19970038/152-20). In the document there is a compilation of 6 case scenarios for answering questions as the following: 

    2.1. [which payer satisfies maximum net asset value test](https://github.com/LogicalContracts/LogicalEnglish/blob/b36ce6e39ccc6ca29ab62397f7f1a5ab7799b8f7/kb/1_net_asset_value_test_2.pl#L517). 

    2.2. [the net value of the CGT assets of which entity is which amount](https://github.com/LogicalContracts/LogicalEnglish/blob/b36ce6e39ccc6ca29ab62397f7f1a5ab7799b8f7/kb/1_net_asset_value_test_2.pl#L520).

    2.3. [which partner is connected with which other partner](https://github.com/LogicalContracts/LogicalEnglish/blob/b36ce6e39ccc6ca29ab62397f7f1a5ab7799b8f7/kb/1_net_asset_value_test_2.pl#L523). 

    2.4. [which first entity controls which second entity](https://github.com/LogicalContracts/LogicalEnglish/blob/b36ce6e39ccc6ca29ab62397f7f1a5ab7799b8f7/kb/1_net_asset_value_test_2.pl#L526).

The case scenarios are:

    ## Example 1: Colin operates a newsagency as a sole trader.
   
    Colin operates a newsagency as a sole trader.
    Colin's son, Simon, carries on his own florist business, which is unrelated to the newsagency business. 
    Simon owns the land and building from which the newsagency is conducted and leases it to Colin. 
    Simon also owns 100 percent of the shares in Simco Pty Ltd, which carries on a separate business. 
    Simon is connected with Simco Pty Ltd because he controls the company. 
    Simon regularly consults Colin for advice in his business affairs and acts according 
    to Colin’s wishes – therefore, Simon is Colin’s affiliate.

    To determine whether he satisfies the maximum net asset value test, 
    Colin includes the market value of the land and building owned by Simon 
    (because it is used in his newsagency business) but does not include Simon’s other assets 
    used in his florist business (because they are not used in the newsagency business). 
    Nor does Colin include Simco’s assets, because those assets are not used in his business 
    and Simco Pty Ltd is only connected because of his affiliate, Simon.
 
        scenario Colin is:
            the CGT assets of Colin are in [the land, the building].
            the land is in the assets of Simon.
            the building is in the assets of Simon.
            Simon is connected with Simco Pty Ltd.
            Simon is an affiliate of Colin.
            10000000 is the market value of the land.
            10000000 is the market value of the building.
            5000000 is a liability of the land.
            5000000 is a liability of the building.
            2500000 is a provision for i.
            2500000 is a provision for ii.
            2500000 is a provision for iii.
            2500000 is a provision for iv. 

    ## Example 2
   
    Ben owns a house that has a market value of $750,000 just before applying the net assets test. 
    Ben has owned the house for 12 years:
   
       for the first three years, 20% of it was used for producing assessable income
       for the following two years, 40% was used for producing assessable income
       for two years, it was used solely as a main residence
       for the last five years, 10% was used for producing assessable income.
   
    Ben’s dwelling has had 15.8% income-producing use:
    
       3 ÷ 12 years × 20 = 5.0
       2 ÷ 12 years × 40 = 6.7
       2 ÷ 12 years × 0 = 0.0
       5 ÷ 12 years × 10 = 4.1
   
    Ben will include $118,500 in his net assets ($750,000 × 15.8%).
   
    Ben has a liability of $500,000 attached to the house, therefore 15.8% ($79,000) 
    of the liability is also included in the calculation of net assets.

        scenario Ben is:
            Ben is an individual.
            the CGT assets of Ben are in [the house].
            the house is in the assets of Ben.
            750000 is the market value of the house.
            500000 is a liability of the house.
            the house is a dwelling. 
            for 3 out of 12 times 20 percent the house was used for producing assessable income.
            for 2 out of 12 times 40 percent the house was used for producing assessable income.
            for 2 out of 12 times 0 percent the house was used for producing assessable income.
            for 5 out of 12 times 10 percent the house was used for producing assessable income.

    ## Example 3
    
    Cool Tool Pty Ltd is selling its business. The assets and liabilities of the company are as follows:
    
    Assets:
   	
    Plant and machinery   $1,500,000
    Freehold premises     $3,500,000
                          __________
    Total assets          $5,000,000
    
    Liabilities:
    	
    Mortgage (secured over the premises)  $2,000,000
    Provision for leave of employees        $500,000
    Provision for rebates                   $200,000
    Provision for possible damages payout   $100,000
                                          __________
    Total liabilities                     $2,800,000
    
    Net assets:                           $2,200,000
   
    The net value of the CGT assets of the company is calculated as follows:
    Assets:
   	
    Plant and machinery   $1,500,000
    Freehold premises     $3,500,000
                          __________
    Total assets          $5,000,000
    
    Liabilities:
    	
    Mortgage (secured over the premises)  $2,000,000
    Provision for leave of employees        $500,000
                                          __________
    Total liabilities                     $2,500,000
   
    Net assets:                           $2,500,000

        scenario Cool Tool Pty Ltd is:
            the CGT assets of Cool Tool Pty Ltd are in [plant and machinery, freehold premises].
            plant and machinery is in the assets of Cool Tool Pty Ltd.
            freehold premises is in the assets of Cool Tool Pty Ltd.
            1500000 is the market value of plant and machinery. 
            3500000 is the market value of freehold premises.
            2000000 is a liability of freehold premises.
            500000 is a provision for i. 

   
    ## Example: Personal use assets (4)
   
    The market value of Lana’s CGT assets is:
    
    Land used in business                 $50,000
    Business goodwill                    $200,000
    Trading stock	                       $100,000
    Plant                                 $50,000
    Boat (used solely for personal use)   $50,000
    Home                                 $600,000
                                        _________
    Total                             $ 1,050,000
    
    Lana borrowed $20,000 to buy the boat.
    
    When working out the net value of her CGT assets, Lana does not include:
    
        the market value of her boat ($50,000)
        the liability for the boat.
    
    Lana uses 50 percent of her home exclusively for income-producing activity. 
    She includes 50 percent of the value of her home, representing the 
    income-producing percentage, and does not include the other 50 percent ($300,000).
    
    Therefore, the net value of her CGT assets is:
    
    $1,050,000 − $350,000 = $700,000

        scenario Lana is:
            Lana is an individual.
            the CGT assets of Lana are in [land, goodwill, stock, plant, boat, home].
            land is in the assets of Lana.
            goodwill is in the assets of Lana.
            stock is in the assets of Lana.
            plant is in the assets of Lana.
            boat is in the assets of Lana.
            home is in the assets of Lana.
            50000 is the market value of land.
            200000 is the market value of goodwill.
            100000 is the market value of stock.
            50000 is the market value of plant.
            50000 is the market value of boat.
            600000 is the market value of home. 
            20000 is a liability of boat.
            for 12 out of 12 times 50 percent home was used for producing assessable income.
            home is a dwelling.
            boat is being used solely for the personal use and enjoyment of Lana or Lana's affiliate. 

    ## Example 5

    Olivia and Jill conduct a professional practice in partnership. 
    As they each have a 50% interest in the partnership, they each control 
    the partnership. Therefore, the partnership is connected with each partner, 
    and Olivia and Jill are each connected with the partnership.
    
        scenario Olivia and Jill is:
            Olivia controls the partnership.
            Jill controls the partnership. 


    ## Example 7
   
    Lachlan owns 48 percent of the shares in Ayoubi Art Supplies. He plays no part in the day-to-day or 
    strategic decision-making of the business. Daniel owns 42 percent of the shares in the company. 
    The remaining 10 percent of shares are beneficially owned by a third shareholder who does not take 
    part in the management of the business. All shares carry the same voting rights and Daniel makes 
    all day-to-day and strategic decisions for the company. Even though Lachlan owns 48 percent of 
    the shares in Ayoubi Art Supplies, he would not be taken to control the company if the 
    Commissioner was satisfied that the company is controlled by Daniel.

    scenario Ayoubi Art Supplies is:
        Lachlan is an individual.
        Daniel is an individual.
        Ayoubi Art Supplies is a company. 
        Lachlan owns 48 percent of the shares in Ayoubi Art Supplies. 
        Daniel owns 42 percent of the shares in Ayoubi Art Supplies.
        Ayoubi Art Supplies acts in accordance with directions from Daniel.
        all shares carry the same voting rights.     
        the shares is a share. 
        the commissioner is satisfied that
            Ayoubi Art Supplies is controlled by Daniel. 



3. [The Data Protection Policy](../../kb/data_protection_policy.pl) Example is a document to formalize the rules to verify that certain principles of data protection are upheld within an organization. A [video](https://youtu.be/A3wUUTpHlyc) explains how to read the document. 


# A [JavaScript API for LE](../../clientExample)

3.1. [Example Code with the Bristish National Act](../../clientExample/src/LogicalEnglish.js)

3.2. [Example Code with CGT Assets and Exemptions](../../clientExample/src/LodgeITLE.js)

