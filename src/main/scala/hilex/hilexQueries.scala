package hilex

import java.sql.ResultSet

import org.skife.jdbi.v2.Handle
import org.skife.jdbi.v2.Query

import database.DatabaseUtilities._
import database.DatabaseUtilities
import spark.TestSpark

object hilexQueries {

  val getLemma = GetResult[Lemma](r => Lemma(r.getString("modern_lemma"),
    r.getInt("lemma_id"),
    r.getString("persistent_id"),
    r.getString("lemma_part_of_speech")))


  def bind[T](q: AlmostQuery[T], n: String, v: String): AlmostQuery[T] = db => q(db).bind(n, v)

  val pos = "ADP"

  val dataSchema = if (TestSpark.atHome) "data" else "gigant_hilex_data"
  val senseSchema = "wnt_ids"

  def lemmaQueryExample(): AlmostQuery[Lemma] = {
    db =>
      db.createQuery(
        s"""
      select modern_lemma, lemma_id,persistent_id, lemma_part_of_speech from data.lemmata
        where lemma_part_of_speech ~ '${pos}'""").map(getLemma)
  }

  def lemmaQueryWhere(where: String): AlmostQuery[Lemma] = {

    db =>
      db.createQuery(
        s"""
      select modern_lemma, lemma_id,persistent_id, lemma_part_of_speech from ${dataSchema}.lemmata
        where ${where}""").map(getLemma)
  }

  def intValues(l: List[Int]) = "(" + l.mkString(",") + ")"

  def stringValues(l: List[String]) = "('" + l.mkString("','") + "')"

  def getWordforms(lemmata: List[Lemma]): AlmostQuery[Wordform] = {
    val ids = lemmata.map(_.lemma_id)
    val lemmaMap = lemmata.map(l => (l.lemma_id, l)).toMap
    implicit val makeWordform = GetResult[Wordform](
      r => Wordform(lemmaMap(r.getInt("lemma_id")),
        r.getInt("analyzed_wordform_id"),
        r.getString("wordform"))
    )
    val q =
      s"""
        select
          lemma_id, analyzed_wordform_id, wordform
        from
            ${dataSchema}.analyzed_wordforms a,
            ${dataSchema}.wordforms w
        where a.wordform_id=w.wordform_id and lemma_id in """ + intValues(ids)

    db => db.createQuery(q).map(makeWordform)
  }

  def getWordformGroups(lemmata: List[Lemma]): AlmostQuery[Wordform] = {
    val ids = lemmata.map(_.lemma_id)
    val lemmaMap = lemmata.map(l => (l.lemma_id, l)).toMap
    implicit val makeWordform = GetResult[Wordform](
      r => Wordform(lemmaMap(r.getInt("lemma_id")),
        r.getInt("analyzed_wordform_id"),
        r.getString("wordform"))
    )
    val q =
      s"""
        select
          lemma_id, analyzed_wordform_id, wordform
        from
            ${dataSchema}.analyzed_wordforms a,
            ${dataSchema}.wordforms w
        where a.wordform_id=w.wordform_id and lemma_id in """ + intValues(ids)

    db => db.createQuery(q).map(makeWordform)
  }
  def getSenses(lemmata: List[Lemma]): AlmostQuery[Sense] = {
    val ids = lemmata.map(_.persistent_id)
    val lemmaMap = lemmata.map(l => (l.persistent_id, l)).toMap

    val makeSense = GetResult[Sense](
      r => Sense(lemmaMap(r.getString("lemma_id")),
        r.getString("persistent_id"),
        r.getString("lemma_id"),
        r.getString("parent_sense_id"),
        r.getString("definition"),
        r.getString("type"))
    )
    val q =
      s"""
          select distinct lemma_id, persistent_id, lemma_id, parent_sense_id , definition, type
           from ${senseSchema}.senses
           where lemma_id in """ + stringValues(ids)
    db => db.createQuery(q).map(makeSense)
  }

  val makeSense = GetResult[Sense](
    r => Sense(
      getLemmaByPersistentId(r.getString("lemma_id")),
      r.getString("persistent_id"),
      r.getString("lemma_id"),
      r.getString("parent_sense_id"),
      r.getString("definition"),
      r.getString("type"))
  )

  def getSubSenses(senses: List[Sense]): AlmostQuery[Sense] = {
    val ids = senses.map(_.persistent_id)

    val q =
      s"""
          select lemma_id, persistent_id, lemma_id, parent_sense_id , definition, type
           from ${senseSchema}.senses
           where parent_sense_id in """ + stringValues(ids)

    db => db.createQuery(q).map(makeSense)
  }

  def getParentSenses(senses: List[Sense]): AlmostQuery[Sense] = {
    val ids = senses.map(_.parent_sense_id)

    val q =
      s"""
          select lemma_id, persistent_id, lemma_id, parent_sense_id , definition, type
           from ${senseSchema}.senses
           where persistent_id in """ + stringValues(ids)
    db => db.createQuery(q).map(makeSense)
  }

  def getAttestationsBelow(s: Sense): List[Attestation] =
    s.attestations ++ s.subSenses.flatMap(s => getAttestationsBelow(s))

  def getAttestationsBelow(s: Sense, max: Int): List[Attestation] = {
    val all = s.attestations ++ s.subSenses.flatMap(s => getAttestationsBelow(s, max))
    all.take(max)
  }

  def getDocument(r: ResultSet): DocumentMetadata = {
    val items =
      List(
        ("author", r.getString("author")),
        ("title", r.getString("title")),
        ("year_from", r.getInt("year_from").toString),
        ("year_to", r.getInt("year_to").toString),
        ("dictionary", r.getString("dictionary"))
      ).toMap
    DocumentMetadata("bla", items)
  }

  def getAttestation(r: ResultSet): Attestation = {
    val a = Attestation(getWordform(r.getInt("analyzed_wordform_id")),
      r.getString("quote"),
      r.getInt("start_pos"),
      r.getInt("end_pos"), r.getString("eg_id"), getDocument(r))
    a.copy(document = a.document.copy(persistent_id = a.eg_id))
  }

  def getAttestations(wordforms: List[Wordform]): AlmostQuery[Attestation] = {
    val ids = wordforms.map(_.analyzed_wordform_id)
    val wordformMap = wordforms.map(l => (l.analyzed_wordform_id, l)).toMap

    implicit val makeAttestation = GetResult[Attestation](
      r => getAttestation(r))

    val q =
      s"""
      select
           a.analyzed_wordform_id,
           quote,
           start_pos,
           end_pos,
           eg_id,
           d.author,
           d.title,
           d.year_from,
           d.year_to,
           d.dictionary
      from
           #${dataSchema}.analyzed_wordforms a, #${dataSchema}.token_attestations t, wnt_ids.documents d
      where
             d.document_id=t.document_id
            and a.analyzed_wordform_id=t.analyzed_wordform_id
            and t.analyzed_wordform_id in """ + intValues(ids)
    db => db.createQuery(q).map(makeAttestation)
  }

  def getLemma(lemma_id: Int): Lemma = {
    val l = Hilex.slurp(hilexQueries.lemmaQueryWhere(s"lemma_id='${lemma_id}'"))
    if (l.isEmpty)
      null
    else
      l.head
  }

  def getLemmaByPersistentId(lemma_id: String): Lemma = {
    val l = Hilex.slurp(hilexQueries.lemmaQueryWhere(s"persistent_id='${lemma_id}'"))
    if (l.isEmpty)
      null
    else
      l.head
  }

  def getSense(sense_id: String): Sense = {

    val q =
      s"""
          select distinct lemma_id, persistent_id, lemma_id, parent_sense_id , definition, type
           from ${senseSchema}.senses
           where persistent_id='${sense_id}' """

    def x(db: Handle):Query[Sense] = db.createQuery(q).map(makeSense)
    Hilex.slurp(db => x(db)).head
  }

  def getLemmaWithPoS(lemma: String, pos: String): List[Lemma] = {
    val q: AlmostQuery[Lemma] = db => db.createQuery(
      s"""
      select modern_lemma, lemma_id,persistent_id, lemma_part_of_speech from ${dataSchema}.lemmata
        where lower(modern_lemma)=lower(:lemma) and lemma_part_of_speech ~ :pos""")
      .bind("lemma", lemma)
      .bind("pos", pos)
      .map(getLemma)
    Hilex.slurp(q)
  }

  implicit def getWordform(analyzed_wordform_id: Int): Wordform = {
    implicit val makeWordform = GetResult[Wordform](
      r => Wordform(getLemma(r.getInt("lemma_id")), r.getInt("analyzed_wordform_id"), r.getString("wordform"))
    )
    val q =
      s"""
        select
          lemma_id, analyzed_wordform_id, wordform
        from
            ${dataSchema}.analyzed_wordforms a, ${dataSchema}.wordforms w
        where
        a.wordform_id=w.wordform_id and analyzed_wordform_id=${analyzed_wordform_id} """

    val a = (db: Handle) => db.createQuery(q).map(makeWordform)

    val l: List[Wordform] = Hilex.slurp(a)
    if (l.isEmpty)
      null
    else
      l.head
  }

  def getSynonymDefinitions(sense: Sense): List[SynonymDefinition] = {

    implicit val makeSynonymDefinition = GetResult[SynonymDefinition](
      r => SynonymDefinition(r.getString("sense_id"), r.getString("syn")))
    val q =
      s"""
         select distinct
            sense_id, syn
         from
           wnt.modsyn
          where
           sense_id=:sense_id
        """
    val a: AlmostQuery[SynonymDefinition] = (db: Handle) => db.createQuery(q).bind("sense_id", sense.persistent_id).map(makeSynonymDefinition)
    val l: List[SynonymDefinition] = DatabaseUtilities.slurp(Hilex.diamantRuwDB, a)
    l
  }

  def getAttestationsForSense(senses: List[Sense]): AlmostQuery[Attestation] = {
    val ids = senses.map(_.persistent_id)
    val senseMap = senses.map(l => (l.persistent_id, l)).toMap
    implicit val makeAttestation = GetResult[Attestation](r => getAttestation(r))

    val q =
      s"""
      select distinct
           analyzed_wordform_id,
           quote, start_pos,
           end_pos,
           d.eg_id,
           d.author,
           d.title,
           d.year_from,
           d.year_to,
           d.dictionary
      from
            ${dataSchema}.token_attestations t, ${senseSchema}.eg_sense e, ${senseSchema}.documents d
      where
            d.eg_id = e.eg_id
            and t.document_id=d.document_id
            and e.sense_id in """ + stringValues(ids)
    val a = (db: Handle) => db.createQuery(q).map(makeAttestation)
    a
  }
  case class ServletRow(lempos: String, senseId: String, example:String, properties: String)

  val QueryForServlet = Select(r =>1,
    from=
      """
        |select distinct
        |           l.modern_lemma + ":" + l.lemma_part_of_speech,
        |           quote, start_pos,
        |           end_pos,
        |           d.eg_id,
        |           d.author,
        |           d.title,
        |           d.year_from,
        |           d.year_to,
        |           d.dictionary
        |      from
        |            ${senseSchema}.senses s,
        |            ${dataSchema}.token_attestations t,
        |            ${dataSchema}.lemmata l,
        |            ${senseSchema}.eg_sense e,
        |            ${senseSchema}.documents d
        |      where
        |            d.eg_id = e.eg_id
        |            and e.sense_id=s.sense_id
        |            and s.lemma_id=l.persistent_id
        |            and t.document_id=d.document_id
      """.stripMargin
  )
  val romanOrArabic = (s: Sense) => List("roman", "arabic").contains(s.sense_type) || s.parent_sense_id == null
  // as in: concat(sql"select id from USERS where id in ", values(Seq(1,2))).as[Int]

  val createSimple =
    """
      |create temporary table part_of_group
      |select distinct
      |        analyzed_wordforms.analyzed_wordform_id, documents.persistent_id
      |from
      |        wordform_groups,
      |        token_attestations,
      |        analyzed_wordforms,
      |        documents
      |where
      |        wordform_groups.document_id = token_attestations.document_id and
      |        wordform_groups.onset = token_attestations.start_pos and
      |        analyzed_wordforms.analyzed_wordform_id = token_attestations.analyzed_wordform_id and
      |        documents.document_id=token_attestations.document_id;
      |alter table part_of_group add index(analyzed_wordform_id);
      |drop table if exists simple_analyzed_wordforms;
      |create table simple_analyzed_wordforms
      |select * from analyzed_wordforms
      |where not (analyzed_wordform_id in (select analyzed_wordform_id from part_of_group));
      |alter table simple_analyzed_wordforms add index(analyzed_wordform_id);
      |alter table simple_analyzed_wordforms add index(lemma_id);
      |alter table simple_analyzed_wordforms add index(wordform_id);
      |
    """.stripMargin
}
