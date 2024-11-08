import pandas as pd
import polars as pl
import re
from janitor import pivot_longer
from typing import Iterable

def main()->None:
    '''
    Get tennis match data from Jeff Sackmann's github and transform into
    data at the player-match level using pandas and polars.
    '''
    years = range(2003,2024)
    matches_pd = pd_load_matches(years)
    player_matches_pd = pd_get_player_matches(matches_pd)

    matches_pl = pl_load_matches(years)
    player_matches_pl = pl_get_player_matches(matches_pl)

def pd_load_matches(years:Iterable[int])->pd.DataFrame:
    '''
    Load matches from github and return pandas DataFrame.
    '''
    match_datasets = [
        pd.read_csv(f"https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_{i}.csv") for i in years
    ]
    matches = pd.concat(match_datasets)
    return matches

def pl_load_matches(years:Iterable[int])->pl.LazyFrame:
    '''
    Lazily load matches from github and return polars LazyFrame.
    '''
    match_datasets = [
        pl.scan_csv(
            f"https://raw.githubusercontent.com"+\
            f"/JeffSackmann/tennis_atp/master/atp_matches_{i}.csv",
            infer_schema_length=int(1e9)
        ) for i in years
    ]
    matches = pl.concat(match_datasets)
    return matches

def pd_get_player_matches(match_df:pd.DataFrame)->pd.DataFrame:
    '''
    Take match dataset, add some fields, and pivot longer to return data
    at the level of player-match using pandas functions.
    '''
    matches = (
        match_df
        .assign(
            match_id = lambda df: df['tourney_date'].astype('str') + '-' + df['winner_id'].astype(str) + 
                '-' + df['loser_id'].astype(str),
            year = lambda df: list(map(lambda x: x[0:4],df['tourney_date'].astype('str')))
        )
        .rename(
            columns = lambda x: x.replace('1st','first').replace('2nd','second').replace('l_','loser_'),
            inplace = False
        )
        .rename(
            columns = lambda x: x if x == 'draw_size' else x.replace('w_','winner_')
        )
        .assign(
            num_tiebreaks = lambda df: list(
                map(
                    lambda x: len(re.findall('\\(',x)),
                    df['score']
                )
            ),
            winner_tiebreaks_won = lambda df: list(
                map(
                    lambda x: len(re.findall('7-6\\(',x)),
                    df['score']
                )
            ),
            loser_tiebreaks_won = lambda df: list(
                map(
                    lambda x: len(re.findall('6-7\\(',x)),
                    df['score']
                )
            )
        )
    )

    # pyjanitor pivot_longer() version
    # player_matches = matches.pivot_longer( # get player_matches
    #     column_names = [x for x in matches.columns if len(re.findall(w_l_regex,x)) != 0],
    #     names_to = ['result','.value'],
    #     names_pattern = r'(winner|loser)_(.+)'    
    # )

    matches_long = (
        pd.melt(
            matches,
            id_vars = [i for i in matches.columns if len(i) == len(i.replace('winner_','').replace('loser_',''))],
            var_name = 'metric',
            value_name = 'value'
        )
        .assign(
            result = lambda df: [
                'winner' if 'winner' in i else 'loser' for i in df['metric']
            ],
            metric = lambda df: [
                i.replace('winner_','').replace('loser_','') for i in df['metric']
            ]
        )
    )

    player_matches = (
        pd.pivot(
            matches_long,
            index = [i for i in matches_long.columns if i not in ['metric','value']],
            columns = 'metric',
            values = 'value'
        ).reset_index()
    )

    return player_matches

def pl_get_player_matches(match_df:pl.LazyFrame)->pl.DataFrame:
    '''
    Take match dataset, add some fields, and pivot longer to return data
    at the level of player-match using polars functions.
    '''
    matches_pl = (
        match_df
        .rename(
            mapping = lambda x: x if x == 'draw_size' else x.replace('1st','first')
                                .replace('2nd','second').replace('l_','loser_').replace('w_','winner_')
        )
        .with_columns(
            match_id = pl.col('tourney_date').cast(pl.String) + '-' + 
                    pl.col('winner_id').cast(pl.String) + '-' + 
                    pl.col('loser_id').cast(pl.String),
            num_tiebreaks = pl.col('score').str.extract_all(r'\(').list.len().cast(pl.Int64),
            winner_tiebreaks_won = pl.col('score').str.extract_all(r'7-6\(').list.len(),
            loser_tiebreaks_won = pl.col('score').str.extract_all(r'6-7\(').list.len(),
            year = pl.col('tourney_date').cast(pl.String).str.slice(0,length=4).cast(pl.Int64)
        )
    )

    player_matches_pl = (
        matches_pl
        .unpivot(
            index = pl.col("*").exclude("^.*(winner|loser).*$")
        )
        .with_columns(
            result = pl.col('variable').str.extract('^(winner|loser)'),
            variable = pl.col('variable').str.replace('^(winner_|loser_)','')
        )
        .collect() # LazyFrames don't have a pivot() method 
        .pivot(
            on='variable',
            values='value'
        )
    )

    return player_matches_pl

if __name__ == '__main__':
    main()

