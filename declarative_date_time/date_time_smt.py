# import itertools as it
import z3

def constrain_vars(solver, day, day_var, month, month_var, year, year_var):
  constraint = z3.And(
    1900 <= year_var, year_var <= 2200,
    1 <= month_var, month_var <= 12,
    1 <= day_var, day_var <= 31,
    z3.Implies(
      z3.Or(month_var == 4, month_var == 6, month_var == 9, month_var == 12),
      day_var <= 30
    ),
    z3.Implies(month_var == 2, day_var <= 29),
    z3.Implies(
      z3.And(month_var == 2, day_var == 29),
      z3.Or(
        year_var % 400 == 0,
        z3.And(year_var % 4 == 0, year_var % 100 != 0)
      )
    )
  )

  solver.add(constraint)

  for (x, y) in [(day, day_var), (month, month_var), (year, year_var)]:
    if isinstance(x, int):
      solver.add(y == z3.IntVal(x))

def valid_date_smt(day, month, year):
  day_var = z3.FreshInt('day')
  month_var = z3.FreshInt('month')
  year_var = z3.FreshInt('year')

  solver = z3.Solver()

  constrain_vars(solver, day, day_var, month, month_var, year, year_var)

  while (solver.check() == z3.sat):
    model = solver.model()
    yield [
      model[day_var].as_long(),
      model[month_var].as_long(),
      model[year_var].as_long()
    ]

    solver.add(
      z3.Or([var != model[var] for var in [day_var, month_var, year_var]])
    )

def valid_date_pair_smt(day0, month0, year0, day1, month1, year1):
  day0_var = z3.FreshInt('day')
  month0_var = z3.FreshInt('month')
  year0_var = z3.FreshInt('year')

  day1_var = z3.FreshInt('day')
  month1_var = z3.FreshInt('month')
  year1_var = z3.FreshInt('year')

  solver = z3.Solver()

  constrain_vars(solver, day0, day0_var, month0, month0_var, year0, year0_var)
  constrain_vars(solver, day1, day1_var, month1, month1_var, year1, year1_var)

  lexical_ordering_constraint = z3.And(
    year0_var <= year1_var,
    z3.Implies(year0_var == year1_var, month0_var <= month1_var),
    z3.Implies(
      z3.And(
        year0_var == year1_var,
        month0_var == month1_var,
      ),
      day0_var <= day1_var)
  )

  solver.add(lexical_ordering_constraint)

  while (solver.check() == z3.sat):
    model = solver.model()
    yield [
      model[day0_var].as_long(),
      model[month0_var].as_long(),
      model[year0_var].as_long(),
      model[day1_var].as_long(),
      model[month1_var].as_long(),
      model[year1_var].as_long()
    ]

    solver.add(
      z3.Or([var != model[var] for var in [
        day0_var, month0_var, year0_var,
        day1_var, month1_var, year1_var
      ]])
    )

# print(list(it.islice(is_valid_date('d', 'm', 'y'), 100)))
