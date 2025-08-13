# Community-Funded Film Projects Smart Contract

This Clarity smart contract enables filmmakers to raise funds for their projects through community backing, with profit-sharing mechanisms for backers.

## Overview

The Film Funding contract allows:
- Filmmakers to create funding campaigns for their film projects
- Community members to back films with STX tokens
- Automatic profit distribution based on backing percentage
- Refunds if funding goals aren't met

## Contract Functions

### For Filmmakers

#### Create a Film Project
```clarity
(create-film film-id title description funding-goal min-contribution deadline)
```
- `film-id`: Unique identifier for the film
- `title`: Film title (max 100 ASCII characters)
- `description`: Film description (max 500 ASCII characters)
- `funding-goal`: Minimum amount needed in microSTX
- `min-contribution`: Minimum backing amount in microSTX
- `deadline`: Block height when funding closes

#### Close Funding
```clarity
(close-funding film-id)
```
Ends the funding period. Can be called by the creator before the deadline or by anyone after.

#### Add Film Profit
```clarity
(add-film-profit film-id profit-amount)
```
Adds profit to be distributed to backers based on their share.

### For Backers

#### Back a Film
```clarity
(back-film film-id amount)
```
Contribute to a film project. Amount must be at least the minimum contribution.

#### Claim Refund
```clarity
(claim-refund film-id)
```
If funding goal wasn't met, backers can claim refunds.

#### Claim Profit Share
```clarity
(claim-profit-share film-id)
```
Claim your share of profits based on your backing percentage.

### Read-Only Functions

- `get-film`: Get film details
- `get-backer-info`: Get information about a backer's contribution
- `get-film-profit`: Get profit information for a film
- `is-film-active`: Check if a film's funding period is active
- `calculate-profit-share`: Calculate a backer's profit share

## Example Usage

1. Filmmaker creates a new film project:
```clarity
(contract-call? .film create-film u1 "Blockchain Documentary" "A documentary about blockchain technology" u1000000000 u10000000 u10000)
```

2. Backers contribute to the film:
```clarity
(contract-call? .film back-film u1 u50000000)
```

3. After deadline, funding is closed:
```clarity
(contract-call? .film close-funding u1)
```

4. If funded, filmmaker adds profits:
```clarity
(contract-call? .film add-film-profit u1 u500000000)
```

5. Backers claim their profit share:
```clarity
(contract-call? .film claim-profit-share u1)
```

## Error Codes

- `u100`: Not contract owner
- `u101`: Film ID already exists
- `u102`: Film not found
- `u103`: Unauthorized action
- `u104`: Funding period closed
- `u105`: Funding still active
- `u106`: Insufficient funds
- `u107`: Minimum funding not met
- `u108`: Already claimed
- `u109`: Not a backer
- `u110`: Zero amount not allowed