package org.matsim.episim.model;

import com.google.inject.Inject;
import org.matsim.api.core.v01.Id;
import org.matsim.api.core.v01.population.Person;
import org.matsim.episim.EpisimPerson;
import org.matsim.episim.EpisimUtils;
import org.matsim.episim.VaccinationConfigGroup;

import java.time.LocalDate;
import java.util.*;

/**
 * Vaccinate people starting with oldest first
 */
public class VaccinationByAge implements VaccinationModel {

	private final SplittableRandom rnd;
	private final VaccinationConfigGroup vaccinationConfig;

	private final static int MAX_AGE = 130;
	private final static int MINIMUM_AGE_FOR_VACCINATIONS = 0;

	@Inject
	public VaccinationByAge(SplittableRandom rnd, VaccinationConfigGroup vaccinationConfig) {
		this.rnd = rnd;
		this.vaccinationConfig = vaccinationConfig;
	}

	@Override
	public int handleVaccination(Map<Id<Person>, EpisimPerson> persons, boolean reVaccination, int availableVaccinations, LocalDate date, int iteration, double now) {
		if (availableVaccinations == 0)
			return 0;

		Map<VaccinationType, Double> prob = vaccinationConfig.getVaccinationTypeProb(date);

		// perAge is an ArrayList where we have for each age (in years) an
		// ArrayList of Persons that are qualified for a vaccination
		final List<EpisimPerson>[] perAge = new List[MAX_AGE];

		for (int i = 0; i < MAX_AGE; i++)
			perAge[i] = new ArrayList<>();

		for (EpisimPerson p : persons.values()) {
			if (
					p.isVaccinable() &&
					p.getDiseaseStatus() == EpisimPerson.DiseaseStatus.susceptible && !p.isRecentlyRecovered(iteration) &&
					(p.getVaccinationStatus() == (reVaccination ? EpisimPerson.VaccinationStatus.yes : EpisimPerson.VaccinationStatus.no) ) &&
					(p.getReVaccinationStatus() == EpisimPerson.VaccinationStatus.no) &&
					(reVaccination ? p.daysSince(EpisimPerson.VaccinationStatus.yes, iteration) >= vaccinationConfig.getParams(p.getVaccinationType()).getBoostWaitPeriod() : true))
			{

				perAge[p.getAge()].add(p);
			}
		}

		int age = MAX_AGE - 1;
		int vaccinationsLeft = availableVaccinations;

		while (vaccinationsLeft > 0 && age > MINIMUM_AGE_FOR_VACCINATIONS) {

			List<EpisimPerson> candidates = perAge[age];

			// list is shuffled to avoid eventual bias
			if (candidates.size() > vaccinationsLeft)
				Collections.shuffle(perAge[age], new Random(EpisimUtils.getSeed(rnd)));

			for (int i = 0; i < Math.min(candidates.size(), vaccinationsLeft); i++) {
				EpisimPerson person = candidates.get(i);
				vaccinate(person, iteration, reVaccination ? null : VaccinationModel.chooseVaccinationType(prob, rnd), reVaccination);
				vaccinationsLeft--;
			}

			age--;
		}

		return availableVaccinations - vaccinationsLeft;
	}
}
